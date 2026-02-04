# ==============================================================================
# 5. UI DEFINITION
# ==============================================================================

# --- Composants UI ---

ui_tab_home <- nav_panel(
  title = "Accueil",
  icon = icon("chart-line"),
  card(
    class = "hero-section mb-4", fill = FALSE,
    div(
      h3("Bienvenue sur JobMatch Data 🎯", class = "mt-0"),
      p("Explorez, analysez et matchez avec les meilleures offres Data du marché.", style="font-size: 1.1em; opacity: 0.9;"),
      div(class = "text-start mt-3",
          actionButton("go_to_match", "Matcher mon CV 🔥", class = "btn-light text-primary fw-bold me-2", style="border-radius: 30px; padding: 8px 20px;"),
          actionButton("go_to_search", "Recherche classique 🔍", class = "btn-outline-light", style="border-radius: 30px; padding: 8px 20px;")
      )
    )
  ),
  layout_columns(
    fill = FALSE,
    value_box(title = "Offres Recensées", value = nrow(df_offres), showcase = icon("database"), theme = "primary"),
    value_box(title = "Salaire Moyen", value = if(any(!is.na(df_offres$Salaire_Moyen))) paste0(round(mean(df_offres$Salaire_Moyen, na.rm=TRUE)/1000, 1), " k€") else "N/A", showcase = icon("money-bill-wave"), theme = "success"),
    value_box(title = "Top Ville", value = if(nrow(df_offres) > 0) names(sort(table(df_offres$Lieu), decreasing=TRUE))[1] else "N/A", showcase = icon("map-marker-alt"), theme = "purple")
  ),
  layout_columns(
    col_widths = c(8, 4), fill = FALSE,
    card(fill = FALSE, card_header("🏆 Top 10 des entreprises qui recrutent le plus"), plotOutput("plot_top_companies", height = "400px")),
    card(fill = FALSE, card_header("☁️ Compétences les plus demandées"), wordcloud2Output("plot_cloud", height = "400px"))
  )
)

ui_tab_search <- nav_panel(
  title = "Parcourir",
  value = "Parcourir les offres",
  icon = icon("list"),
  layout_sidebar(
    sidebar = sidebar(
      width = "375px",
      title = "Filtres Avancés",
      textInput("search_kw", "Mots-clés", placeholder = "Ex: Python, Senior..."),
      selectizeInput("filter_loc", "Lieu", choices = c("Tous", choix_lieux), options = list(placeholder = "Rechercher une ville...")),
      pickerInput("filter_contract", "Type de contrat", choices = choix_contrats, selected = choix_contrats, multiple = TRUE, options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2", `count-selected-text` = "{0} types", `none-selected-text` = "Aucun")),
      pickerInput("filter_sector", "Secteur d'activité", choices = choix_secteur, selected = choix_secteur, multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE, `none-selected-text` = "Indifférent")),
      pickerInput("filter_remote", "Télétravail", choices = choix_teletravail, selected = choix_teletravail, multiple = TRUE, options = list(`actions-box` = TRUE, `none-selected-text` = "Indifférent")),
      pickerInput("filter_education", "Niveau d'étude", choices = as.character(choix_education), selected = as.character(choix_education), multiple = TRUE, options = list(`actions-box` = TRUE, `none-selected-text` = "Indifférent")),
      sliderInput("filter_exp", "Années d'expérience", min = 0, max = max_exp, value = c(0, max_exp), step = 1),
      sliderInput("filter_salary", "Fourchette de salaire (annuel)", min = 0, max = max_salary, value = c(0, max_salary), step = CONSTANTS$SALARY_STEP, post = " €")
    ),
    card(
      style = "min-height: 80vh;",
      card_header(div(class="d-flex justify-content-between align-items-center", "Liste des offres", uiOutput("result_count"))),
      DTOutput("table_offres")
    )
  )
)

ui_tab_map <- nav_panel(
  title = "Carte",
  icon = icon("map"),
  card(
    full_screen = TRUE,
    style = "min-height: 80vh;",
    card_header("Localisation des offres"),
    card_body(
      padding = 0,
      leafletOutput("offers_map", height = "100%")
    )
  )
)

ui_tab_match <- nav_panel(
  title = "Match avec mon CV",
  value = "Match avec une offre",
  icon = icon("fire"),
  style = "min-height: 85vh;",
  div(
    class = "container-fluid",
    
    # Section 1: Upload CV for AI matching
    card(
      class = "mb-4",
      card_header("📄 Analyser mon CV avec IA"),
      card_body(
        fileInput("cv_file", "Télécharger votre CV (PDF)", 
                  accept = c(".pdf")),
        actionButton("apply_cv_match", "Analyser et matcher", 
                     class = "btn-success"),
        hr(),
        uiOutput("cv_top_matches")
      )
    ),
    
    # Section 2: Skills-based Tinder interface
    layout_sidebar(
      sidebar = sidebar(
        width = "375px",
        title = "🎯 Votre Profil",
        selectizeInput("cv_skills", "Vos compétences clés :", 
                       choices = choix_competences_reels, 
                       multiple = TRUE, 
                       options = list(create = TRUE, 
                                      placeholder = "Ex: Python, AWS...", 
                                      plugins = list('remove_button'))),
        helpText("Le deck d'offres sera trié pour afficher les meilleures correspondances en premier."),
        actionButton("apply_cv_sort", "Trier les offres", 
                     class="btn-primary w-100")
      ),
      div(
        style = "max-width: 700px; margin: 0 auto;",
        div(class = "text-end mb-2", 
            actionButton("undo_btn", "Retour arrière", 
                         icon = icon("undo"), 
                         class = "btn-outline-secondary btn-sm")),
        uiOutput("match_interface")
      )
    )
  )
)



ui_tab_favs <- nav_panel(
  title = "Favoris",
  icon = icon("star"),
  card(
    style = "min-height: 80vh;",
    card_header("Mes offres sauvegardées"),
    div(class="d-flex justify-content-between mb-3",
        downloadButton("download_fav", "Exporter (.csv)", class = "btn-success btn-sm"),
        actionButton("clear_fav", "Vider les favoris", icon = icon("trash"), class = "btn-danger btn-sm")
    ),
    DTOutput("table_favoris")
  )
)


# --- Assemblage UI ---

page_navbar(
  title = "JobMatch Data 🎯",
  id = "nav_main",
  theme = CONSTANTS$THEME,
  fillable = FALSE,
  
  useShinyjs(),
  tags$head(
    tags$style(CSS_STYLES),
    tags$script(JS_SCRIPTS$SMART_NAVBAR),
    tags$script(JS_SCRIPTS$KEYBOARD_SHORTCUTS)
  ),
  
  ui_tab_home,
  ui_tab_search,
  ui_tab_map,
  ui_tab_match,
  ui_tab_favs
)
