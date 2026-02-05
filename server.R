library(here)

Sys.setenv(
  RETICULATE_PYTHON = here("pip_env", "Scripts", "python.exe")
)

# ==============================================================================
# 0. Get python classes
# ==============================================================================
source_python("classes/my_classes.py")

# ==============================================================================
# 6. SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {
  
  # --- Reactive Values ---
  vals <- reactiveValues(
    favoris_ids = c(),
    current_index = 1,
    show_success_view = FALSE,
    sorted_indices = 1:nrow(df_offres),
    last_removed_from_favs = NULL,
    undo_notification_id = NULL,
    top_matches = NULL
  )
  
  # ===========================================================================
  # REACTIVE EXPRESSIONS (Computed once per invalidation)
  # ===========================================================================
  
  # --- Filtered Data (used by multiple outputs) ---
  filtered_data <- reactive({
    data <- df_offres
    
    # Keyword filter
    if (!is.null(input$search_kw) && input$search_kw != "") {
      kw <- tolower(input$search_kw)
      data <- data %>% 
        filter(
          grepl(kw, tolower(Intitule)) | 
            grepl(kw, tolower(Description)) | 
            grepl(kw, tolower(Competences_Clean))
        )
    }
    
    # Location filter
    if (!is.null(input$filter_loc) && input$filter_loc != "Tous") {
      data <- data %>% filter(Lieu == input$filter_loc)
    }
    
    # Contract type filter
    if (!is.null(input$filter_contract) && length(input$filter_contract) > 0) {
      data <- data %>% filter(Type_contrat %in% input$filter_contract)
    }
    
    # Remote work filter
    if (!is.null(input$filter_remote) && length(input$filter_remote) > 0) {
      data <- data %>% filter(Teletravail %in% input$filter_remote)
    }
    
    # Education filter
    if (!is.null(input$filter_education) && length(input$filter_education) > 0) {
      data <- data %>% filter(Education %in% input$filter_education)
    }
    
    # Sector filter
    if (!is.null(input$filter_sector) && length(input$filter_sector) > 0) {
      pattern <- paste(input$filter_sector, collapse = "|")
      data <- data %>% filter(grepl(pattern, Secteur))
    }
    
    # Salary range filter
    if (!is.null(input$filter_salary)) {
      data <- data %>% 
        filter(
          is.na(Salaire_Moyen) | 
            (Salaire_Moyen >= input$filter_salary[1] & 
               Salaire_Moyen <= input$filter_salary[2])
        )
    }
    
    # Experience range filter
    if (!is.null(input$filter_exp)) {
      data <- data %>% 
        filter(
          is.na(Experience_Clean) | 
            (Experience_Clean >= input$filter_exp[1] & 
               Experience_Clean <= input$filter_exp[2])
        )
    }
    
    return(data)
  })
  
  # ===========================================================================
  # OBSERVERS (Side effects - navigation, actions)
  # ===========================================================================
  
  # --- Navigation Events ---
  observeEvent(input$go_to_llm, {
    nav_select("nav_main", "Recherche par LLM")
  })
  
  observeEvent(input$go_to_match, {
    nav_select("nav_main", "Match avec une offre")
  })
  
  observeEvent(input$go_to_search, {
    nav_select("nav_main", "Parcourir les offres")
  })
  
  # --- CV Matching (LLM) ---
  observeEvent(input$apply_cv_match, {
    req(input$cv_file)
    
    # Afficher notification de chargement
    showNotification(
      "Analyse en cours... Cela peut prendre quelques instants.",
      id = "cv_processing",
      type = "message",
      duration = NULL
    )
    
    tryCatch({
      matcher <- CVMatcher(MODEL_PATH)
      py_res <- matcher$match_all_jobs(
        cv_pdf = input$cv_file$datapath,
        jobs_csv = "data/jobs.csv",
        top_n = as.integer(5)  # Ensure integer
      )
      
      # Convert to R dataframe and ensure clean indices
      vals$top_matches <- py_res %>% 
        py_to_r() %>%  # Explicit conversion
        as.data.frame()
      
      # Ensure row names are NULL
      rownames(vals$top_matches) <- NULL
      
      # Retirer la notification de chargement
      removeNotification("cv_processing")
      
      showNotification(
        "✅ CV analysé avec succès! Les meilleures correspondances sont affichées ci-dessous.", 
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      removeNotification("cv_processing")
      showNotification(
        paste("❌ Erreur lors de l'analyse:", e$message), 
        type = "error",
        duration = 10
      )
      print(paste("Debug error:", e$message))
      reticulate::py_last_error()  # Print Python traceback
    })
  })
  
  
  
  # --- CV Skills Sorting ---
  observeEvent(input$apply_cv_sort, {
    req(input$cv_skills)
    
    tryCatch({
      full_texts <- paste(df_offres$Description, df_offres$Competences_Clean)
      scores <- calculer_scores_vectorized(full_texts, input$cv_skills)
      vals$sorted_indices <- order(scores, decreasing = TRUE)
      vals$current_index <- 1
      vals$show_success_view <- FALSE
      showNotification("Offres triées selon vos compétences !", type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur de tri:", e$message), type = "error")
    })
  })
  
  # --- Favorites Management ---
  observeEvent(input$fav_click_id, {
    clicked_id <- as.integer(input$fav_click_id)
    
    if (clicked_id %in% vals$favoris_ids) {
      # Remove from favorites
      vals$favoris_ids <- vals$favoris_ids[vals$favoris_ids != clicked_id]
      vals$last_removed_from_favs <- clicked_id
      vals$undo_notification_id <- showNotification(
        "Retiré des favoris.", 
        type = "warning", 
        duration = 4,
        action = actionButton("undo_fav_remove", "Annuler")
      )
    } else {
      # Add to favorites
      vals$favoris_ids <- c(vals$favoris_ids, clicked_id)
      showNotification("Ajouté aux favoris ⭐", type = "message", duration = 2)
    }
  })
  
  # --- Undo Favorite Removal ---
  observeEvent(input$undo_fav_remove, {
    if (!is.null(vals$last_removed_from_favs)) {
      vals$favoris_ids <- c(vals$favoris_ids, vals$last_removed_from_favs)
      vals$last_removed_from_favs <- NULL
      
      if (!is.null(vals$undo_notification_id)) {
        removeNotification(vals$undo_notification_id)
        vals$undo_notification_id <- NULL
      }
      
      showNotification("Action annulée.", type = "default")
    }
  })
  
  # --- View Job Details Modal ---
  observeEvent(input$view_click_id, {
    offre <- df_offres %>% filter(ID == as.integer(input$view_click_id))
    
    if (nrow(offre) > 0) {
      showModal(create_job_modal(offre))
    }
  })
  
  # --- Tinder-style Navigation ---
  observeEvent(input$like_btn, {
    if (vals$current_index <= length(vals$sorted_indices)) {
      current_id <- df_offres$ID[vals$sorted_indices[vals$current_index]]
      
      if (!(current_id %in% vals$favoris_ids)) {
        vals$favoris_ids <- c(vals$favoris_ids, current_id)
      }
      
      vals$show_success_view <- TRUE
    }
  })
  
  observeEvent(input$pass_btn, {
    vals$current_index <- vals$current_index + 1
    vals$show_success_view <- FALSE
  })
  
  observeEvent(input$continue_btn, {
    vals$current_index <- vals$current_index + 1
    vals$show_success_view <- FALSE
  })
  
  observeEvent(input$undo_btn, {
    if (vals$show_success_view) {
      vals$show_success_view <- FALSE
    } else if (vals$current_index > 1) {
      vals$current_index <- vals$current_index - 1
    }
  })
  
  # --- Clear Favorites ---
  observeEvent(input$clear_fav, {
    vals$favoris_ids <- c()
    showNotification("Favoris vidés.", type = "warning")
  })
  
  # ===========================================================================
  # OUTPUTS (Rendering)
  # ===========================================================================
  
  # --- Home Tab: Top Companies Plot ---
  output$plot_top_companies <- renderPlot({
    req(df_offres)
    
    top_c <- df_offres %>%
      count(Entreprise, sort = TRUE) %>%
      filter(Entreprise != "N/A" & Entreprise != "") %>%
      head(10)
    
    if (nrow(top_c) == 0) {
      plot.new()
      text(0.5, 0.5, "Aucune donnée disponible", cex = 1.5)
    } else {
      ggplot(top_c, aes(x = reorder(Entreprise, n), y = n)) +
        geom_col(fill = "#6f42c1") +
        coord_flip() +
        theme_minimal(base_size = 14) +
        labs(x = "", y = "Nombre d'offres") +
        geom_text(aes(label = n), hjust = -0.2, size = 4)
    }
  }, res = 96)
  
  # --- Home Tab: Word Cloud ---
  output$plot_cloud <- renderWordcloud2({
    req(df_words_cloud)
    req(nrow(df_words_cloud) > 0)
    
    wordcloud2(
      df_words_cloud, 
      size = 0.8, 
      color = "random-dark", 
      shape = 'circle'
    )
  })
  
  # --- Browse Tab: Result Count Badge ---
  output$result_count <- renderUI({
    count <- nrow(filtered_data())
    span(
      class = "badge bg-secondary", 
      paste(count, "résultat(s)")
    )
  })
  
  # --- Browse Tab: Jobs Table ---
  output$table_offres <- renderDT({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    # Prepare display data with action buttons
    display_df <- prepare_table_display_data(data, vals$favoris_ids)
    
    datatable(
      display_df,
      escape = FALSE,
      selection = 'none',
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = 0)
        )
      )
    )
  })
  
  # --- Match Tab: Match Interface ---
  output$match_interface <- renderUI({
    req(vals$sorted_indices)
    
    # Check if we've reached the end
    if (vals$current_index > length(vals$sorted_indices)) {
      return(
        card(
          class = "bg-light text-center p-5",
          h2("🎉 C'est fini !"),
          p("Plus d'offres à voir.")
        )
      )
    }
    
    # Get current offer
    offre <- df_offres[vals$sorted_indices[vals$current_index], ]
    
    if (vals$show_success_view) {
      # Success view after liking
      card(
        class = "shadow-lg border-success",
        style = "border-radius: 15px;",
        div(
          class = "p-3 text-center bg-success text-white",
          style = "border-radius: 15px 15px 0 0;",
          icon("check-circle", class = "fa-3x mb-2"),
          h3("C'est un Match !"),
          p("Offre sauvegardée.")
        ),
        card_body(
          h2(offre$Intitule, class = "text-center text-primary"),
          h4(
            paste(offre$Entreprise, "•", offre$Secteur),
            class = "text-center text-muted mb-4"
          ),
          layout_columns(
            fill = FALSE,
            value_box(
              title = "Lieu",
              value = offre$Lieu,
              showcase = icon("map-marker"),
              theme = "light"
            ),
            value_box(
              title = "Contrat",
              value = offre$Contrat_Full,
              showcase = icon("file-contract"),
              theme = "light"
            ),
            value_box(
              title = "Salaire annuel",
              value = if (!is.na(offre$Salaire_Moyen)) {
                paste(format(offre$Salaire_Moyen, big.mark = " "), "€")
              } else {
                "N/A"
              },
              showcase = icon("euro"),
              theme = "light"
            )
          ),
          hr(),
          h5("Description complète :"),
          div(
            style = "max-height: 400px; overflow-y: auto; background-color: #f8f9fa; padding: 15px; border-radius: 10px;",
            p(offre$Description)
          )
        ),
        card_footer(
          class = "d-flex justify-content-between align-items-center p-3",
          a(
            href = offre$Lien_Annonce,
            target = "_blank",
            class = "btn btn-outline-secondary",
            "Voir l'offre originale"
          ),
          a(
            href = offre$Lien_Final,
            target = "_blank",
            class = "btn btn-success btn-lg",
            icon("paper-plane"),
            " POSTULER"
          ),
          actionButton(
            "continue_btn",
            "Offre suivante (Entrée) →",
            class = "btn btn-outline-primary btn-lg"
          )
        )
      )
    } else {
      # Tinder-style card view
      full_offer_text <- paste(offre$Description, offre$Competences_Clean)
      
      matches_count <- if (!is.null(input$cv_skills) && length(input$cv_skills) > 0) {
        calculer_score_cv(full_offer_text, input$cv_skills)
      } else {
        0
      }
      
      score_match <- if (!is.null(input$cv_skills) && length(input$cv_skills) > 0) {
        round((matches_count / length(input$cv_skills)) * 100)
      } else {
        0
      }
      
      tagList(
        card(
          class = "shadow-lg",
          style = "border-radius: 15px; background: linear-gradient(135deg, #fdfbfb 0%, #ebedee 100%);",
          card_header(
            class = "bg-primary text-white text-center",
            div(
              style = "max-height: 100px; overflow-y: auto;",
              h4(offre$Intitule),
              span(
                style = "font-size: 0.8em",
                paste(offre$Entreprise, "•", offre$Secteur)
              )
            )
          ),
          card_body(
            style = "height: 380px; overflow-y: auto;",
            generer_badges_competences(offre$Competences_Clean),
            creer_barre_score_tech(score_match),
            h5("📍 Lieu : ", offre$Lieu),
            if (!is.na(offre$Salaire_Moyen)) {
              h5("💰 Salaire : ", paste(format(offre$Salaire_Moyen, big.mark = " "), "€"))
            },
            hr(),
            p(substr(offre$Description, 1, 350), "...")
          ),
          card_footer(
            class = "text-center",
            a(
              href = offre$Lien_Annonce,
              target = "_blank",
              class = "btn btn-outline-primary btn-sm",
              "Voir l'offre originale"
            )
          )
        ),
        div(
          style = "display: flex; justify-content: center; gap: 30px; margin-top: 20px;",
          actionButton(
            "pass_btn",
            "",
            icon = icon("times", class = "fa-2x"),
            title = "Passer (←)",
            class = "btn-circle-lg btn-pass"
          ),
          actionButton(
            "like_btn",
            "",
            icon = icon("heart", class = "fa-2x"),
            title = "Aimer (→)",
            class = "btn-circle-lg btn-like"
          )
        )
      )
    }
  })
  
  # --- Map Tab: Initial Map ---
  output$offers_map <- renderLeaflet({
    data_init <- isolate(filtered_data()) %>%
      filter(!is.na(Latitude) & !is.na(Longitude))
    
    map <- leaflet() %>%
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(minZoom = 2, maxZoom = 18)
      )
    
    if (nrow(data_init) > 0) {
      map <- map %>%
        addCircleMarkers(
          data = data_init,
          lng = ~Longitude,
          lat = ~Latitude,
          radius = 6,
          stroke = FALSE,
          fillOpacity = 0.7,
          fillColor = "#0d6efd",
          popup = generate_map_popup_content(data_init),
          clusterOptions = markerClusterOptions()
        ) %>%
        fitBounds(
          lng1 = min(data_init$Longitude),
          lat1 = min(data_init$Latitude),
          lng2 = max(data_init$Longitude),
          lat2 = max(data_init$Latitude)
        )
    } else {
      map <- map %>% setView(lng = 2.35, lat = 48.85, zoom = 5)
    }
    
    map
  })
  
  # --- Map Tab: Update Map on Filter Changes ---
  observe({
    data_to_plot <- filtered_data() %>%
      filter(!is.na(Latitude) & !is.na(Longitude))
    
    if (nrow(data_to_plot) == 0) {
      leafletProxy("offers_map") %>%
        clearMarkers() %>%
        clearMarkerClusters()
      return()
    }
    
    popup_content <- generate_map_popup_content(data_to_plot)
    
    leafletProxy("offers_map", data = data_to_plot) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 6,
        stroke = FALSE,
        fillOpacity = 0.7,
        fillColor = "#0d6efd",
        popup = popup_content,
        clusterOptions = markerClusterOptions()
      ) %>%
      fitBounds(
        lng1 = min(data_to_plot$Longitude),
        lat1 = min(data_to_plot$Latitude),
        lng2 = max(data_to_plot$Longitude),
        lat2 = max(data_to_plot$Latitude)
      )
  })
  
  # --- Favorites Tab: Favorites Table ---
  output$table_favoris <- renderDT({
    if (length(vals$favoris_ids) == 0) {
      return(
        datatable(
          data.frame(Message = "Aucun favori pour le moment."),
          options = list(dom = 't'),
          rownames = FALSE
        )
      )
    }
    
    df_fav <- df_offres %>%
      filter(ID %in% vals$favoris_ids)
    
    display_df <- prepare_table_display_data(df_fav, vals$favoris_ids)
    
    datatable(
      display_df,
      escape = FALSE,
      selection = 'none',
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = 0)
        )
      )
    )
  })
  
  # --- Favorites Tab: Download Handler ---
  output$download_fav <- downloadHandler(
    filename = function() {
      paste0("mes_favoris_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df_offres %>%
        filter(ID %in% vals$favoris_ids) %>%
        write.csv(file, row.names = FALSE)
    }
  )
  
  # --- LLM Tab: Top 5 CV Matches (from Python model) ---
  output$cv_top_matches <- renderUI({
    # État par défaut: message d'invitation
    if (is.null(vals$top_matches)) {
      return(
        div(
          class = "text-center py-5",
          icon("cloud-upload-alt", class = "fa-4x text-muted mb-3"),
          h4("Aucune analyse effectuée", class = "text-muted"),
          p("Uploadez votre CV et cliquez sur 'Lancer l'analyse IA' pour découvrir les offres qui vous correspondent le mieux.")
        )
      )
    }
    
    df <- vals$top_matches
    
    # Ensure it's a proper R dataframe with integer row indices
    if (!is.data.frame(df)) {
      df <- as.data.frame(df)
    }
    
    # Reset row names to be safe
    rownames(df) <- NULL
    
    # Check if dataframe has rows
    if (nrow(df) == 0) {
      return(
        div(
          class = "text-center py-5",
          icon("search", class = "fa-4x text-warning mb-3"),
          h4("Aucune correspondance trouvée", class = "text-muted"),
          p("Le modèle n'a pas trouvé d'offres correspondant à votre profil. Essayez avec un autre CV.")
        )
      )
    }
    
    tagList(
      div(
        class = "alert alert-success d-flex align-items-center mb-4",
        icon("check-circle", class = "me-2"),
        strong(paste0("Analyse terminée! ", nrow(df), " offres correspondent à votre profil."))
      ),
      lapply(seq_len(nrow(df)), function(i) {
        # Get the job ID
        job_id <- if("ID" %in% colnames(df)) {
          df$ID[i]
        } else {
          # Fallback: find matching job in df_offres by title
          matching_job <- df_offres %>% 
            filter(Intitule == df$intitule_poste[i]) %>% 
            slice(1)
          if(nrow(matching_job) > 0) matching_job$ID else NA
        }
        
        # Déterminer la couleur du badge selon le score
        score_val <- round(as.numeric(df$similarity_score[i]), 2)
        badge_color <- if(score_val >= 80) {
          "success"
        } else if(score_val >= 60) {
          "info"
        } else {
          "warning"
        }
        
        card(
          class = "mb-3 shadow-sm border-0",
          style = "border-left: 4px solid #28a745 !important;",
          card_body(
            div(
              class = "d-flex justify-content-between align-items-start mb-2",
              h5(
                class = "mb-0",
                span(class = paste0("badge bg-", badge_color, " me-2"), 
                     paste0("#", i)),
                as.character(df$intitule_poste[i])
              ),
              span(
                class = paste0("badge bg-", badge_color),
                style = "font-size: 1em;",
                paste0(score_val, "% match")
              )
            ),
            
            if("entreprise" %in% colnames(df)) {
              p(class = "mb-2",
                icon("building", class = "text-primary me-1"),
                strong("Entreprise : "), 
                as.character(df$entreprise[i]))
            },
            
            if("ville" %in% colnames(df)) {
              p(class = "mb-2",
                icon("map-marker-alt", class = "text-danger me-1"),
                strong("Lieu : "), 
                as.character(df$ville[i]))
            },
            
            div(
              class = "mb-3 p-3 bg-light rounded",
              p(class = "mb-0 small", 
                substr(as.character(df$description[i]), 1, 300), 
                if(nchar(as.character(df$description[i])) > 300) "..." else "")
            ),
            
            div(
              class = "d-flex gap-2 flex-wrap",
              if(!is.na(job_id)) {
                actionButton(
                  paste0("view_match_", job_id),
                  "Voir les détails complets",
                  icon = icon("eye"),
                  class = "btn-primary btn-sm",
                  onclick = paste0("Shiny.setInputValue('view_click_id', ", 
                                   job_id, ", {priority: 'event'})")
                )
              },
              
              if("lien_annonce" %in% colnames(df) && !is.na(df$lien_annonce[i])) {
                a(
                  href = as.character(df$lien_annonce[i]),
                  target = "_blank",
                  class = "btn btn-outline-primary btn-sm",
                  icon("external-link-alt"),
                  " Annonce originale"
                )
              },
              
              if(!is.na(job_id)) {
                actionButton(
                  paste0("add_fav_match_", job_id),
                  "Ajouter aux favoris",
                  icon = icon("star"),
                  class = if(job_id %in% vals$favoris_ids) "btn-warning btn-sm" else "btn-outline-warning btn-sm",
                  onclick = paste0("Shiny.setInputValue('fav_click_id', ", 
                                   job_id, ", {priority: 'event'})")
                )
              }
            )
          )
        )
      })
    )
  })
  
}