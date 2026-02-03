# ==============================================================================
# 6. SERVER LOGIC
# ==============================================================================

function(input, output, session) {
  
  # --- Valeurs réactives ---
  vals <- reactiveValues(
    favoris_ids = c(),
    current_index = 1,
    show_success_view = FALSE,
    sorted_indices = 1:nrow(df_offres),
    last_removed_from_favs = NULL,
    undo_notification_id = NULL
  )
  
  # --- Navigation ---
  observeEvent(input$go_to_match, { nav_select("nav_main", "Match avec une offre") })
  observeEvent(input$go_to_search, { nav_select("nav_main", "Parcourir les offres") })
  
  # --- Onglet Accueil (Plots) ---
  output$plot_top_companies <- renderPlot({
    top_c <- df_offres %>% 
      count(Entreprise, sort = TRUE) %>% 
      filter(Entreprise != "N/A" & Entreprise != "") %>% 
      head(10)
    
    if(nrow(top_c) == 0) {
      plot.new(); text(0.5, 0.5, "Aucune donnée disponible", cex = 1.5)
    } else {
      ggplot(top_c, aes(x = reorder(Entreprise, n), y = n)) + 
        geom_col(fill = "#6f42c1") + 
        coord_flip() + 
        theme_minimal(base_size = 14) + 
        labs(x = "", y = "Nombre d'offres") + 
        geom_text(aes(label = n), hjust = -0.2, size = 4)
    }
  }, res = 96)
  
  output$plot_cloud <- renderWordcloud2({
    req(nrow(df_words_cloud) > 0)
    wordcloud2(df_words_cloud, size = 0.8, color = "random-dark", shape = 'circle')
  })
  
  # --- Onglet Parcourir (Filtres & Table) ---
  
  filtered_data <- reactive({
    data <- df_offres
    
    # Filtre mot-clé
    if (input$search_kw != "") {
      kw <- tolower(input$search_kw)
      data <- data %>% filter(
        grepl(kw, tolower(Intitule)) | 
        grepl(kw, tolower(Description)) |
        grepl(kw, tolower(Competences_Clean))
      )
    }
    
    # Filtres catégoriels
    if (input$filter_loc != "Tous") data <- data %>% filter(Lieu == input$filter_loc)
    if (!is.null(input$filter_contract)) data <- data %>% filter(Type_contrat %in% input$filter_contract)
    if (!is.null(input$filter_remote)) data <- data %>% filter(Teletravail %in% input$filter_remote)
    if (!is.null(input$filter_education)) data <- data %>% filter(Education %in% input$filter_education)
    
    # Filtre secteur
    if (!is.null(input$filter_sector)) {
      pattern <- paste(input$filter_sector, collapse = "|")
      data <- data %>% filter(grepl(pattern, Secteur))
    }
    
    # Filtres numériques
    data <- data %>% filter(is.na(Salaire_Moyen) | (Salaire_Moyen >= input$filter_salary[1] & Salaire_Moyen <= input$filter_salary[2]))
    data <- data %>% filter(is.na(Experience_Clean) | (Experience_Clean >= input$filter_exp[1] & Experience_Clean <= input$filter_exp[2]))
    
    data
  })
  
  output$result_count <- renderUI({ 
    span(class="badge bg-secondary", paste(nrow(filtered_data()), "résultat(s)")) 
  })
  
  output$table_offres <- renderDT({
    # Utilisation du helper pour préparer les données avec les boutons HTML
    display_df <- prepare_table_display_data(filtered_data(), vals$favoris_ids)
    
    datatable(
      display_df, escape = FALSE, selection = 'none', rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE, columnDefs = list(list(className = 'dt-center', targets = 0)))
    )
  })
  
  # --- Gestion des Favoris & Modale ---
  
  observeEvent(input$fav_click_id, {
    clicked_id <- as.integer(input$fav_click_id)
    if (clicked_id %in% vals$favoris_ids) {
      vals$favoris_ids <- vals$favoris_ids[vals$favoris_ids != clicked_id]
      vals$last_removed_from_favs <- clicked_id
      vals$undo_notification_id <- showNotification("Retiré des favoris.", type = "warning", duration = 4,
                       action = actionButton("undo_fav_remove", "Annuler"))
    } else {
      vals$favoris_ids <- c(vals$favoris_ids, clicked_id)
      showNotification("Ajouté aux favoris ⭐", type = "message", duration = 2)
    }
  })
  
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
  
  observeEvent(input$view_click_id, {
    offre <- df_offres %>% filter(ID == as.integer(input$view_click_id))
    showModal(create_job_modal(offre))
  })
  
  # --- Onglet Match avec CV ---
  
  observeEvent(input$apply_cv_sort, {
    req(input$cv_skills)
    full_texts <- paste(df_offres$Description, df_offres$Competences_Clean)
    scores <- calculer_scores_vectorized(full_texts, input$cv_skills)
    vals$sorted_indices <- order(scores, decreasing = TRUE)
    vals$current_index <- 1
    vals$show_success_view <- FALSE
    showNotification("Offres triées selon vos compétences !", type = "message")
  })
  
  output$match_interface <- renderUI({
    if (vals$current_index > length(vals$sorted_indices)) {
      return(card(class = "bg-light text-center p-5", h2("🎉 C'est fini !"), p("Plus d'offres à voir.")))
    }
    
    offre <- df_offres[vals$sorted_indices[vals$current_index], ]
    
    if (vals$show_success_view) {
      # Vue "Match Success"
      card(
        class = "shadow-lg border-success", style = "border-radius: 15px;",
        div(class = "p-3 text-center bg-success text-white", style = "border-radius: 15px 15px 0 0;", icon("check-circle", class = "fa-3x mb-2"), h3("C'est un Match !"), p("Offre sauvegardée.")),
        card_body(
          h2(offre$Intitule, class="text-center text-primary"), h4(paste(offre$Entreprise, "•", offre$Secteur), class="text-center text-muted mb-4"),
          layout_columns(fill = FALSE, value_box(title = "Lieu", value = offre$Lieu, showcase = icon("map-marker"), theme = "light"), value_box(title = "Contrat", value = offre$Contrat_Full, showcase = icon("file-contract"), theme = "light"), value_box(title = "Salaire annuel", value = if(!is.na(offre$Salaire_Moyen)) paste(format(offre$Salaire_Moyen, big.mark=" "), "€") else "N/A", showcase = icon("euro"), theme = "light")),
          hr(), h5("Description complète :"), div(style = "max-height: 400px; overflow-y: auto; background-color: #f8f9fa; padding: 15px; border-radius: 10px;", p(offre$Description))
        ),
        card_footer(class = "d-flex justify-content-between align-items-center p-3",
          a(href = offre$Lien_Annonce, target = "_blank", class = "btn btn-outline-secondary", "Voir l'offre originale"),
          a(href = offre$Lien_Final, target = "_blank", class = "btn btn-success btn-lg", icon("paper-plane"), " POSTULER"),
          actionButton("continue_btn", "Offre suivante (Entrée) →", class = "btn btn-outline-primary btn-lg")
        )
      )
    } else {
      # Vue "Tinder Card"
      full_offer_text <- paste(offre$Description, offre$Competences_Clean)
      matches_count <- calculer_score_cv(full_offer_text, input$cv_skills)
      score_match <- if (length(input$cv_skills) > 0) round((matches_count / length(input$cv_skills)) * 100) else 0
      
      tagList(
        card(
          class = "shadow-lg", style = "border-radius: 15px; background: linear-gradient(135deg, #fdfbfb 0%, #ebedee 100%);",
          card_header(class = "bg-primary text-white text-center", div(style = "max-height: 100px; overflow-y: auto;", h4(offre$Intitule), span(style="font-size: 0.8em", paste(offre$Entreprise, "•", offre$Secteur)))),
          card_body(
            style="height: 380px; overflow-y: auto;",
            generer_badges_competences(offre$Competences_Clean),
            creer_barre_score_tech(score_match),
            h5("📍 Lieu : ", offre$Lieu),
            if(!is.na(offre$Salaire_Moyen)) h5("💰 Salaire : ", paste(format(offre$Salaire_Moyen, big.mark=" "), "€")) else NULL,
            hr(), p(substr(offre$Description, 1, 350), "...")
          ),
          card_footer(class = "text-center", a(href = offre$Lien_Annonce, target = "_blank", class = "btn btn-outline-primary btn-sm", "Voir l'offre originale"))
        ),
        div(style = "display: flex; justify-content: center; align-items: center; gap: 30px; margin-top: 20px; width: 100%;",
            actionButton("pass_btn", "", icon = icon("times", class = "fa-2x"), title = "Passer (←)", class = "btn-circle-lg btn-pass"),
            actionButton("like_btn", "", icon = icon("heart", class = "fa-2x"), title = "Aimer (→)", class = "btn-circle-lg btn-like"))
      )
    }
  })
  
  observeEvent(input$like_btn, {
    current_id <- df_offres$ID[vals$sorted_indices[vals$current_index]]
    if (!(current_id %in% vals$favoris_ids)) {
      vals$favoris_ids <- c(vals$favoris_ids, current_id)
    }
    vals$show_success_view <- TRUE
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
  
  # --- Onglet Carte ---
  
  output$offers_map <- renderLeaflet({
    data_init <- isolate(filtered_data()) %>% filter(!is.na(Latitude) & !is.na(Longitude))
    
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(minZoom = 2, maxZoom = 18))
      
    if (nrow(data_init) > 0) {
      map <- map %>%
        addCircleMarkers(
          data = data_init, lng = ~Longitude, lat = ~Latitude,
          radius = 6, stroke = FALSE, fillOpacity = 0.7, fillColor = "#0d6efd",
          popup = generate_map_popup_content(data_init),
          clusterOptions = markerClusterOptions()
        ) %>%
        fitBounds(lng1 = min(data_init$Longitude), lat1 = min(data_init$Latitude),
                  lng2 = max(data_init$Longitude), lat2 = max(data_init$Latitude))
    } else {
      map <- map %>% setView(lng = 2.35, lat = 48.85, zoom = 5)
    }
    map
  })
  
  observe({
    data_to_plot <- filtered_data() %>% filter(!is.na(Latitude) & !is.na(Longitude))
      
    if (nrow(data_to_plot) == 0) {
      leafletProxy("offers_map") %>% clearMarkers() %>% clearMarkerClusters()
      return()
    }
    
    popup_content <- generate_map_popup_content(data_to_plot)
    
    leafletProxy("offers_map", data = data_to_plot) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        radius = 6, stroke = FALSE, fillOpacity = 0.7, fillColor = "#0d6efd",
        popup = popup_content,
        clusterOptions = markerClusterOptions()
      ) %>%
      fitBounds(lng1 = min(data_to_plot$Longitude), lat1 = min(data_to_plot$Latitude),
                lng2 = max(data_to_plot$Longitude), lat2 = max(data_to_plot$Latitude))
  })
  
  # --- Onglet Favoris ---
  
  output$table_favoris <- renderDT({
    if (length(vals$favoris_ids) == 0) {
      return(datatable(data.frame(Message = "Aucun favori pour le moment."), options = list(dom = 't'), rownames = FALSE))
    }
    df_fav <- df_offres %>% filter(ID %in% vals$favoris_ids)
    display_df <- prepare_table_display_data(df_fav, vals$favoris_ids)
    
    datatable(
      display_df, escape = FALSE, selection = 'none', rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE, columnDefs = list(list(className = 'dt-center', targets = 0)))
    )
  })
  
  output$download_fav <- downloadHandler(
    filename = function() { paste0("mes_favoris_data_", Sys.Date(), ".csv") },
    content = function(file) {
      df_offres %>% filter(ID %in% vals$favoris_ids) %>% write.csv(file, row.names = FALSE)
    }
  )
  
  observeEvent(input$clear_fav, {
    vals$favoris_ids <- c()
    showNotification("Favoris vidés.", type = "warning")
  })
}
