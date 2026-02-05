getwd()

library(here)
Sys.unsetenv("RETICULATE_PYTHON")

# ==============================================================================
# 1. SETUP & LIBRARIES
# ==============================================================================

library(reticulate)
library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(readr)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
library(wordcloud2)
library(stringr)
library(leaflet)
library(htmltools)

#use_python("C:/Users/Lucas/AppData/Local/Programs/Python/Python39/python.exe", required=TRUE)

use_python(here("pip_env", "Scripts", "python.exe"), required = TRUE)

# ==============================================================================
# 0. Get python classes and models
# ==============================================================================

source_python("classes/my_classes.py")

MODEL_PATH <- "model/cv_job_matching.model"
DATA_PATH <- here("data", "jobs.csv")

if (!file.exists(MODEL_PATH)) {
  trainer <- Doc2VecTrainer()
  trainer$train(DATA_PATH, MODEL_PATH)
}

# ==============================================================================
# 2. CONFIGURATION & CONSTANTS
# ==============================================================================

CONSTANTS <- list(
  DATA_FILE = "data/jobs.csv",
  MAX_SALARY_DEFAULT = 150000,
  MAX_EXP_DEFAULT = 20,
  SALARY_STEP = 5000,
  THEME = bs_theme(version = 5, bootswatch = "zephyr", 
                   base_font = font_google("Inter"),
                   heading_font = font_google("Montserrat"))
)

# --- Styles CSS ---
CSS_STYLES <- HTML("
  body { padding-top: 70px; background-color: #f8f9fa; }
  .navbar {
    box-shadow: 0 4px 12px rgba(0,0,0,0.05);
    position: fixed; top: 0; width: 100%; z-index: 1030;
    transition: top 0.4s ease-in-out;
  }
  .navbar-hidden { top: -100px !important; }
  .btn-action-table { 
    border: none; background: transparent; font-size: 1.1em; 
    transition: transform 0.2s; margin-right: 5px; 
  }
  .btn-action-table:hover { transform: scale(1.2); }
  .fav-active { color: #f1c40f; } 
  .fav-inactive { color: #ccc; }
  .view-active { color: #0d6efd; }
  .tech-score-bar { 
    height: 10px; border-radius: 5px; background-color: #e9ecef; 
    margin-top: 5px; overflow: hidden; 
  }
  .tech-score-fill { height: 100%; }
  
  /* Améliorations esthétiques globales */
  .card { border: none; border-radius: 16px; box-shadow: 0 4px 20px rgba(0,0,0,0.05); transition: transform 0.3s ease; }
  .card:hover { transform: translateY(-2px); }
  
  .hero-section {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    border-radius: 20px;
    padding: 2rem;
    box-shadow: 0 10px 25px rgba(118, 75, 162, 0.3);
  }
  
  .btn-circle-lg {
    width: 80px; height: 80px; border-radius: 50%; border: none; color: white;
    display: flex; align-items: center; justify-content: center;
    box-shadow: 0 10px 20px rgba(0,0,0,0.15); transition: all 0.3s cubic-bezier(0.175, 0.885, 0.32, 1.275);
  }
  .btn-circle-lg:hover { transform: scale(1.1); box-shadow: 0 15px 30px rgba(0,0,0,0.2); }
  .btn-pass { background: linear-gradient(135deg, #ff6b6b, #ee5253); }
  .btn-like { background: linear-gradient(135deg, #1dd1a1, #10ac84); }
  
  /* Style pour la carte Leaflet */
  .leaflet-popup-content-wrapper { border-radius: 5px; }
  .leaflet-popup-content { font-size: 14px; }
")

# --- Scripts JS ---
JS_SCRIPTS <- list(
  SMART_NAVBAR = HTML("
    $(function() {
      var lastScrollTop = 0;
      $(window).scroll(function(event){
         var st = $(this).scrollTop();
         var navbar = $('.navbar');
         if (st > lastScrollTop && st > 10){
             navbar.addClass('navbar-hidden');
         } else {
             navbar.removeClass('navbar-hidden');
         }
         lastScrollTop = st;
      });
    });
  "),
  KEYBOARD_SHORTCUTS = HTML("
    $(document).on('keydown', function(e) {
      if(e.keyCode == 37 && $('#pass_btn').is(':visible')) { $('#pass_btn').click(); } // Left Arrow
      if(e.keyCode == 39 && $('#like_btn').is(':visible')) { $('#like_btn').click(); } // Right Arrow
      if(e.keyCode == 13 && $('#continue_btn').is(':visible')) { $('#continue_btn').click(); } // Enter
    });
  ")
)

# ==============================================================================
# 3. HELPER FUNCTIONS (LOGIC & UI GENERATION)
# ==============================================================================

# --- Logique Métier ---

#' Calcule un score de correspondance (Vectorisé)
calculer_scores_vectorized <- function(textes_offres, liste_tags_cv) {
  if (is.null(liste_tags_cv) || length(liste_tags_cv) == 0) {
    return(rep(0, length(textes_offres)))
  }
  
  textes_offres_lower <- tolower(textes_offres)
  tags_lower <- tolower(liste_tags_cv)
  
  match_matrix <- sapply(tags_lower, function(tag) {
    safe_tag <- gsub("([+.#])", "\\\\\\1", tag)
    pattern <- paste0("\\b", safe_tag, "\\b")
    grepl(pattern, textes_offres_lower)
  })
  
  if (is.matrix(match_matrix)) rowSums(match_matrix) else as.integer(match_matrix)
}

#' Calcule un score de correspondance (Unitaire)
calculer_score_cv <- function(texte_offre, liste_tags_cv) {
  if (is.null(liste_tags_cv) || length(liste_tags_cv) == 0 || is.na(texte_offre)) return(0)
  texte_offre_lower <- tolower(texte_offre)
  sum(sapply(tolower(liste_tags_cv), function(tag) {
    safe_tag <- gsub("([+.#])", "\\\\\\1", tag)
    pattern <- paste0("\\b", safe_tag, "\\b")
    grepl(pattern, textes_offres_lower)
  }))
}

# --- Générateurs HTML UI ---

generer_badges_competences <- function(texte_competences) {
  if (is.na(texte_competences) || texte_competences == "") return(NULL)
  skills <- trimws(unlist(strsplit(texte_competences, ",")))
  skills <- skills[skills != ""]
  if (length(skills) == 0) return(NULL)
  div(class = "mb-3", lapply(skills, function(k) span(class = "badge bg-info me-1", k)))
}

creer_barre_score_tech <- function(score) {
  score <- min(max(score, 0), 100)
  color <- if (score >= 80) "#2ecc71" else if (score >= 50) "#f1c40f" else "#e74c3c"
  div(class = "mb-3",
    strong(paste0("Score de match : ", score, "%")),
    div(class = "tech-score-bar",
      div(class = "tech-score-fill", style = paste0("width:", score, "%; background:", color, ";"))
    )
  )
}

#' Prépare le dataframe pour l'affichage DT (HTML des boutons inclus)
prepare_table_display_data <- function(data_df, favoris_ids) {
  data_df %>%
    mutate(
      Actions = paste0(
        '<button class="btn-action-table view-active" title="Aperçu rapide" onclick="Shiny.setInputValue(\'view_click_id\', ', ID, ', {priority: \'event\'})"><i class="fa fa-eye"></i></button>',
        if_else(ID %in% favoris_ids,
                paste0('<button class="btn-action-table fav-active" title="Retirer des favoris" onclick="Shiny.setInputValue(\'fav_click_id\', ', ID, ', {priority: \'event\'})"><i class="fa fa-star"></i></button>'),
                paste0('<button class="btn-action-table fav-inactive" title="Ajouter aux favoris" onclick="Shiny.setInputValue(\'fav_click_id\', ', ID, ', {priority: \'event\'})"><i class="far fa-star"></i></button>'))
      ),
      Intitule_Link = paste0("<a href='", Lien_Final, "' target='_blank' style='text-decoration:none; font-weight:bold; color:#333;'>", Intitule, "</a>"),
      Salaire_Fmt = if_else(!is.na(Salaire_Moyen), paste(format(Salaire_Moyen, big.mark=" ", scientific = FALSE), "€"), "N/A"),
      Exp_Fmt = if_else(!is.na(Experience_Clean), paste(Experience_Clean, "an(s)"), "N/A")
    ) %>%
    select(Actions, Intitule_Link, Entreprise, Contrat_Full, Lieu, Salaire_Fmt, Exp_Fmt) %>% 
    rename(" " = Actions, "Poste" = Intitule_Link, "Contrat" = Contrat_Full, "Salaire" = Salaire_Fmt, "Expérience" = Exp_Fmt)
}

#' Génère le contenu HTML des popups de la carte
generate_map_popup_content <- function(data) {
  paste0(
    "<div style='text-align: center; min-width: 150px;'>",
      "<h6 style='margin: 0 0 5px 0; color: #0d6efd; font-weight: bold;'>", htmlEscape(data$Intitule), "</h6>",
      "<div style='font-size: 0.9em; color: #555; margin-bottom: 8px;'>",
        "🏢 ", htmlEscape(data$Entreprise), "<br/>",
        "📍 ", htmlEscape(data$Lieu),
      "</div>",
      "<a href='", data$Lien_Annonce, "' target='_blank' class='btn btn-sm btn-primary w-100' style='color: white; text-decoration: none;'>Voir l'offre</a>",
    "</div>"
  )
}

#' Crée la modale de détails d'une offre
create_job_modal <- function(offre) {
  modalDialog(
    title = offre$Intitule, size = "xl", easyClose = TRUE,
    h4(offre$Entreprise, span(class="text-muted", style="font-size: 0.8em;", paste(" • ", offre$Secteur))),
    layout_columns(
      fill=FALSE, col_widths = 4,
      value_box(title="Lieu", value=offre$Lieu, showcase=icon("map-marker"), theme="light"),
      value_box(title="Contrat", value=offre$Contrat_Full, showcase=icon("file-contract"), theme="light"),
      value_box(title="Salaire annuel", value=if_else(!is.na(offre$Salaire_Moyen), paste(format(offre$Salaire_Moyen, big.mark=" "), "€"), "N/A"), showcase=icon("euro"), theme="light")
    ),
    div(class="mb-3"),
    layout_columns(
      fill=FALSE, col_widths = 4,
      value_box(title="Expérience", value=if_else(!is.na(offre$Experience_Clean), paste(offre$Experience_Clean, "ans"), "N/A"), showcase=icon("briefcase"), theme="light"),
      value_box(title="Télétravail", value=offre$Teletravail, showcase=icon("laptop-house"), theme="light"),
      value_box(title="Éducation", value=offre$Education, showcase=icon("graduation-cap"), theme="light")
    ),
    hr(),
    h5("Compétences requises :"),
    generer_badges_competences(offre$Competences_Clean),
    h5("Description :"),
    div(style="max-height: 300px; overflow-y: auto; background-color: #f8f9fa; padding: 10px; border-radius: 5px;", p(offre$Description)),
    footer = tagList(
      span(class="text-muted me-auto", paste("Publié le :", offre$Date_Publication)),
      a(href = offre$Lien_Annonce, target = "_blank", class = "btn btn-primary", "Voir l'offre originale"),
      actionButton("modal_add_fav", "Ajouter aux Favoris", icon=icon("star"), class="btn-warning", onclick=paste0("Shiny.setInputValue('fav_click_id', ", offre$ID, ", {priority: 'event'})")),
      modalButton("Fermer")
    )
  )
}

# ==============================================================================
# 4. DATA LOADING & PREPARATION
# ==============================================================================

load_and_prepare_data <- function(filepath) {
  if (!file.exists(filepath)) stop("Fichier de données introuvable : ", filepath)
  
  df_raw <- readr::read_delim(filepath, delim = ",", show_col_types = FALSE)
  
  df_raw %>%
    mutate(ID = row_number()) %>%
    transmute(
      ID,
      Intitule = ifelse(is.na(intitule_poste), "N/A", intitule_poste),
      Entreprise = ifelse(is.na(entreprise), "N/A", entreprise),
      Secteur = ifelse(is.na(secteur_entreprise) | secteur_entreprise == "", "Non spécifié", secteur_entreprise),
      Description = description,
      Competences_Clean = ifelse(is.na(competences), "", competences),
      Salaire_Moyen = as.numeric(salaire_moyen),
      Lieu = ifelse(is.na(ville) | ville == "", "Non spécifié", ville),
      Type_contrat = ifelse(is.na(type_emploi) | type_emploi == "", "Non spécifié", type_emploi),
      Contrat_Full = case_when(
        !is.na(type_emploi) & type_emploi != "" & !is.na(duree_contrat) & duree_contrat != "" ~ paste(type_emploi, "-", duree_contrat),
        !is.na(type_emploi) & type_emploi != "" ~ type_emploi,
        !is.na(duree_contrat) & duree_contrat != "" ~ duree_contrat,
        TRUE ~ "Non spécifié"
      ),
      Experience_Clean = as.numeric(experience),
      Lien_Initial = lien_postuler_initial,
      Lien_Final = site_source_final,
      Lien_Annonce = lien_annonce,
      Teletravail = ifelse(is.na(teletravail) | teletravail == "", "Non spécifié", teletravail),
      Education = ifelse(is.na(education) | education == "", "Non spécifié", education),
      Date_Publication = ifelse(is.na(date_publication), "Non spécifié", as.character(date_publication)),
      Latitude = as.numeric(latitude),
      Longitude = as.numeric(longitude)
    )
}

# --- Chargement et Pré-calculs ---
df_offres <- load_and_prepare_data(CONSTANTS$DATA_FILE)

# Helpers pour les filtres
get_unique_sorted <- function(column) {
  vals <- unique(column)
  sort(vals[vals != "" & !is.na(vals)])
}

# Listes de choix
choix_contrats <- get_unique_sorted(df_offres$Type_contrat)
choix_teletravail <- get_unique_sorted(df_offres$Teletravail)
choix_secteur <- get_unique_sorted(trimws(unlist(strsplit(df_offres$Secteur, ","))))
choix_lieux <- df_offres %>% count(Lieu, sort = TRUE) %>% filter(Lieu != "Non spécifié" & !is.na(Lieu)) %>% pull(Lieu)

education_order <- c('Sans diplôme', 'CAP', 'Bac', 'Bac +2', 'Bac +3', 'Bac +4', 'Bac +5', 'Bac +8', 'Non spécifié')
choix_education <- factor(get_unique_sorted(df_offres$Education), levels = intersect(education_order, get_unique_sorted(df_offres$Education)), ordered = TRUE)
choix_education <- sort(choix_education)

# Bornes Sliders
max_salary <- ceiling(max(df_offres$Salaire_Moyen, na.rm = TRUE) / 5000) * 5000
if (!is.finite(max_salary) || max_salary == 0) max_salary <- CONSTANTS$MAX_SALARY_DEFAULT

max_exp <- ceiling(max(df_offres$Experience_Clean, na.rm = TRUE))
if (!is.finite(max_exp) || max_exp == 0) max_exp <- CONSTANTS$MAX_EXP_DEFAULT

# Données Word Cloud
df_words_cloud <- {
  all_skills <- trimws(unlist(strsplit(df_offres$Competences_Clean, ",")))
  all_skills <- all_skills[all_skills != ""]
  if (length(all_skills) > 0) {
    data.frame(table(all_skills)) %>% rename(word = all_skills, freq = Freq) %>% arrange(desc(freq)) %>% head(80)
  } else {
    data.frame(word = character(), freq = numeric())
  }
}
choix_competences_reels <- if (nrow(df_words_cloud) > 0) sort(as.character(df_words_cloud$word)) else character(0)
