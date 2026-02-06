# Projet: Webscraping
# Objet: Définition des fonctions de détection de doublons multi-sites via similarité sémantique
#        (embeddings Sentence-Transformers + recherche de voisins FAISS) et enrichissement du dataset
#        avec les colonnes site_source2..4 / lien_annonce2..4 (+ similarite2..4 optionnel).
# Auteurs: 
# - RAMANANTSALAMA, Antsaniaina Lalatiana
# - HAMMOUCH BOUDJOUDI, Siham
# - COUSSY, Lucas
# - SALOUMI, Leila
# - AJAX--RICAUD, Lenny

from __future__ import annotations

import os
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Tuple

import numpy as np
import pandas as pd

import faiss
from sentence_transformers import SentenceTransformer


# Évite le chargement inutile de TensorFlow / Flax par Transformers
os.environ["TRANSFORMERS_NO_TF"] = "1"
os.environ["TRANSFORMERS_NO_FLAX"] = "1"


@dataclass(frozen=True)
class Config:
    # --------- DOSSIERS / FICHIERS ----------
    project_dir: Path
    data_dirname: str = "data"
    input_filename: str = "jobs.csv"
    output_suffix: str = "_multisite_dupes"   # avant .csv
    # ---------------------------------------

    # --------- COLONNES ----------
    site_col: str = "site_source"
    link_col: str = "lien_annonce"
    text_col: str = "description"
    # -----------------------------

    # --------- FILTRE SITES ----------
    sites: Tuple[str, ...] = ("HelloWork", "Welcome to the Jungle", "France Travail", "Workable")
    # ---------------------------------

    # --------- EMBEDDINGS / MATCH ----------
    model_name: str = "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2"
    sim_threshold: float = 0.88
    top_k: int = 25
    min_desc_len: int = 80
    batch_size: int = 64
    # --------------------------------------

    @property
    def data_dir(self) -> Path:
        return self.project_dir / self.data_dirname

    @property
    def input_path(self) -> Path:
        return self.data_dir / self.input_filename

    @property
    def output_path(self) -> Path:
        stem = Path(self.input_filename).stem
        return self.data_dir / f"{stem}{self.output_suffix}.csv"


def clean_text(value: object) -> str:
    """Nettoyage minimal (HTML, espaces) + lower."""
    if pd.isna(value):
        return ""
    s = str(value)
    s = re.sub(r"<[^>]+>", " ", s)
    s = re.sub(r"\s+", " ", s).strip()
    return s.lower()


def validate_columns(df: pd.DataFrame, required: Tuple[str, ...]) -> None:
    missing = [c for c in required if c not in df.columns]
    if missing:
        raise ValueError(
            f"Colonnes manquantes dans le CSV: {missing}\n"
            f"Colonnes disponibles: {list(df.columns)}"
        )


def build_embeddings(desc_list: list[str], model_name: str, batch_size: int) -> np.ndarray:
    """Encode toutes les descriptions en embeddings normalisés (float32)."""

    model = SentenceTransformer(model_name)
    emb = model.encode(
        desc_list,
        batch_size=batch_size,
        show_progress_bar=True,
        normalize_embeddings=True,
    )
    return np.asarray(emb, dtype="float32")


def build_faiss_index(emb: np.ndarray):
    """Construit un index FAISS en similarité cosine (via produit scalaire)."""
    index = faiss.IndexFlatIP(emb.shape[1])  # IP sur embeddings normalisés = cosine
    index.add(emb)
    return index


def init_output_columns(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    for k in (2, 3, 4):
        out[f"site_source{k}"] = None
        out[f"lien_annonce{k}"] = None
        out[f"similarite{k}"] = np.nan  # optionnel (audit/debug)
    out["doublon_multi_site"] = False
    return out


def find_multisite_duplicates(
    df: pd.DataFrame,
    D: np.ndarray,
    I: np.ndarray,
    *,
    site_col: str,
    link_col: str,
    min_desc_len: int,
    sim_threshold: float,
    top_k: int,
    sites_set: set[str],
) -> pd.DataFrame:
    out = init_output_columns(df)
    n = len(out)

    for i in range(n):
        if int(out.at[i, "desc_len"]) < min_desc_len:
            continue

        site_i = out.at[i, site_col]
        best_by_site: Dict[str, Tuple[float, str]] = {}

        for r in range(1, top_k):  # 0 = soi-même
            j = int(I[i, r])
            sim = float(D[i, r])

            if sim < sim_threshold:
                break

            if int(out.at[j, "desc_len"]) < min_desc_len:
                continue

            site_j = out.at[j, site_col]
            if site_j not in sites_set:
                continue
            if site_j == site_i:
                continue

            lien_j = out.at[j, link_col]
            if pd.isna(lien_j):
                lien_j = ""

            prev = best_by_site.get(site_j)
            if (prev is None) or (sim > prev[0]):
                best_by_site[site_j] = (sim, str(lien_j))

        if not best_by_site:
            continue

        out.at[i, "doublon_multi_site"] = True
        picks = sorted(best_by_site.items(), key=lambda kv: kv[1][0], reverse=True)[:3]

        for out_idx, (site, (sim, lien)) in enumerate(picks, start=2):
            out.at[i, f"site_source{out_idx}"] = site
            out.at[i, f"lien_annonce{out_idx}"] = lien
            out.at[i, f"similarite{out_idx}"] = sim

    return out


def run(cfg: Config) -> Path:
    """
    Exécute le pipeline complet.
    Retourne le chemin du fichier exporté (Path).
    """
    if not cfg.data_dir.exists():
        raise FileNotFoundError(f"Dossier data introuvable: {cfg.data_dir}")

    if not cfg.input_path.exists():
        raise FileNotFoundError(f"Fichier introuvable: {cfg.input_path}")

    # 1) Load + validations
    df = pd.read_csv(cfg.input_path).copy().reset_index(drop=True)
    validate_columns(df, (cfg.site_col, cfg.link_col, cfg.text_col))

    print("Input :", cfg.input_path)
    print("Output:", cfg.output_path)
    print("Sites présents:", sorted(df[cfg.site_col].dropna().unique().tolist()))

    # Nettoyage texte
    df["desc_clean"] = df[cfg.text_col].map(clean_text)
    df["desc_len"] = df["desc_clean"].str.len().fillna(0).astype(int)

    # 2) Embeddings + FAISS
    # Dépendances: pip install -U sentence-transformers faiss-cpu
    emb = build_embeddings(df["desc_clean"].tolist(), cfg.model_name, cfg.batch_size)
    index = build_faiss_index(emb)
    D, I = index.search(emb, cfg.top_k)

    # 3) Matching multi-sites
    df_out = find_multisite_duplicates(
        df,
        D,
        I,
        site_col=cfg.site_col,
        link_col=cfg.link_col,
        min_desc_len=cfg.min_desc_len,
        sim_threshold=cfg.sim_threshold,
        top_k=cfg.top_k,
        sites_set=set(cfg.sites),
    )

    # 4) Export
    df_out.to_csv(cfg.output_path, index=False, encoding="utf-8-sig")

    nb = int(df_out["doublon_multi_site"].sum())
    print(f"Nb annonces avec doublon multi-site: {nb}")

    return cfg.output_path
