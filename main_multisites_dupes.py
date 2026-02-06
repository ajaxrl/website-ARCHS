# Projet: Webscraping
# Objet: Script principal d’exécution — chargement des données (data/jobs.csv),
#        lancement de la détection de doublons multi-sites et export des résultats dans le dossier data/.
# Auteurs: 
# - RAMANANTSALAMA, Antsaniaina Lalatiana
# - HAMMOUCH BOUDJOUDI, Siham
# - COUSSY, Lucas
# - SALOUMI, Leila
# - AJAX--RICAUD, Lenny

from pathlib import Path
from src_multisites_dupes import Config, run

def main() -> None:
    project_dir = Path(__file__).resolve().parent  # dossier du projet (là où est main_multisites_dupes.py)

    cfg = Config(
        project_dir=project_dir,
        data_dirname="data",
        input_filename="jobs.csv",
        output_suffix="_multisite_dupes",
    )

    out_path = run(cfg)
    print(f"Fichier généré: {out_path}")

if __name__ == "__main__":
    main()
