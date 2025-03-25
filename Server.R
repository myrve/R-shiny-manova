server <- function(input, output, session) {
  
  # Data loading
  data <- reactive({
    req(input$dataFile)
    
    # Show loading message
    showNotification("Loading data...", type = "message", duration = NULL, id = "loadingData")
    
    # Read the data
    tryCatch({
      df <- readxl::read_excel(input$dataFile$datapath)
      
      # Check if data is valid
      if (ncol(df) < 2) {
        showNotification("The uploaded file must contain at least two columns.", type = "error")
        return(NULL)
      }
      
      # Remove loading message
      removeNotification(id = "loadingData")
      
      return(df)
    }, error = function(e) {
      removeNotification(id = "loadingData")
      showNotification(paste("Error loading data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Data preview
  output$dataPreview <- DT::renderDataTable({
    req(data())
    DT::datatable(head(data(), 10), 
                  options = list(scrollX = TRUE, pageLength = 5),
                  rownames = FALSE)
  })
  
  # Detect variable types
  detectVariableTypes <- reactive({
    req(data())
    
    df <- data()
    
    if (input$advancedDetection) {
      # Advanced detection (identifies numeric variables with few unique values as potential factors)
      numeric_vars <- names(df)[sapply(df, function(x) {
        is.numeric(x) && length(unique(x)) > 10
      })]
      
      factor_vars <- names(df)[sapply(df, function(x) {
        is.factor(x) || is.character(x) || is.logical(x) || 
          (is.numeric(x) && length(unique(x)) <= 10)
      })]
    } else {
      # Basic detection
      numeric_vars <- names(df)[sapply(df, function(x) is.numeric(x))]
      factor_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x) || is.logical(x))]
    }
    
    return(list(numeric_vars = numeric_vars, factor_vars = factor_vars))
  })
  
  # Update variable selection dropdowns
  observe({
    req(detectVariableTypes())
    
    var_types <- detectVariableTypes()
    
    updateSelectInput(session, "factorVars", choices = var_types$factor_vars)
    updateSelectInput(session, "dependentVars", choices = var_types$numeric_vars)
  })
  
  # Refresh variable types when button is clicked
  observeEvent(input$refreshVarTypes, {
    req(data())
    
    showNotification("Refreshing variable types...", type = "message", duration = 2)
    
    var_types <- detectVariableTypes()
    
    updateSelectInput(session, "factorVars", choices = var_types$factor_vars)
    updateSelectInput(session, "dependentVars", choices = var_types$numeric_vars)
  })
  
  # Generate all possible interactions
  interactions <- reactive({
    req(input$factorVars)
    if (length(input$factorVars) < 2) return(NULL)
    
    # Generate all possible interactions
    result <- list()
    
    # Generate 2-way interactions
    if (length(input$factorVars) >= 2) {
      combs2 <- combn(input$factorVars, 2)
      for (i in 1:ncol(combs2)) {
        result[[length(result) + 1]] <- list(
          terms = combs2[,i],
          label = paste(combs2[,i], collapse = " * "),
          order = 2
        )
      }
    }
    
    # Generate 3-way and higher interactions
    if (length(input$factorVars) >= 3) {
      for (n in 3:length(input$factorVars)) {
        combs <- combn(input$factorVars, n)
        for (i in 1:ncol(combs)) {
          result[[length(result) + 1]] <- list(
            terms = combs[,i],
            label = paste(combs[,i], collapse = " * "),
            order = n
          )
        }
      }
    }
    
    return(result)
  })
  
  manovaResults <- eventReactive(input$runAnalysis, {
    req(data(), input$dependentVars, input$factorVars)
    
    # MANOVA analizi kodunu buraya ekleyin
    # Örnek:
    tryCatch({
      result <- jmv::manova(
        data = data(),
        deps = input$dependentVars,
        factors = input$factorVars,
        modelTerms = c(input$factorVars, sapply(selectedInteractions(), function(x) x$terms)),
        postHoc = if (input$includePostHoc) input$factorVars else NULL,
        postHocCorr = input$postHocCorrection,
        effectSize = TRUE,
        homo = TRUE,
        sphericity = TRUE
      )
      return(result)
    }, error = function(e) {
      showNotification(paste("Error in MANOVA analysis:", e$message), type = "error")
      return(NULL)
    })
  })
  
  
  # Create interaction UI
  output$interactionsUI <- renderUI({
    req(interactions())
    
    interaction_list <- interactions()
    interaction_ids <- paste0("interaction_", 1:length(interaction_list))
    
    checkboxes <- lapply(1:length(interaction_list), function(i) {
      interaction <- interaction_list[[i]]
      div(class = "interaction-item",
          checkboxInput(
            inputId = interaction_ids[i],
            label = tags$span(
              tags$strong(paste0(interaction$order, "-way interaction: ")),
              interaction$label,
              tags$span(style = "color: #6c757d; font-size: 0.9em;", 
                        paste0(" (", paste(interaction$terms, collapse = ", "), ")"))
            ),
            value = FALSE
          )
      )
    })
    
    do.call(tagList, checkboxes)
  })
  
  # Get selected interactions
  selectedInteractions <- reactive({
    req(interactions())
    
    interaction_list <- interactions()
    interaction_ids <- paste0("interaction_", 1:length(interaction_list))
    
    selected <- list()
    for (i in 1:length(interaction_list)) {
      if (!is.null(input[[interaction_ids[i]]]) && input[[interaction_ids[i]]]) {
        selected[[length(selected) + 1]] <- interaction_list[[i]]
      }
    }
    
    return(selected)
  })
  
  # Select all interactions
  observeEvent(input$selectAllInteractions, {
    req(interactions())
    
    interaction_list <- interactions()
    interaction_ids <- paste0("interaction_", 1:length(interaction_list))
    
    for (i in 1:length(interaction_list)) {
      updateCheckboxInput(session, interaction_ids[i], value = TRUE)
    }
  })
  
  # Deselect all interactions
  observeEvent(input$deselectAllInteractions, {
    req(interactions())
    
    interaction_list <- interactions()
    interaction_ids <- paste0("interaction_", 1:length(interaction_list))
    
    for (i in 1:length(interaction_list)) {
      updateCheckboxInput(session, interaction_ids[i], value = FALSE)
    }
  })
  
  # Download results
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("manova-results-", format(Sys.time(), "%Y%m%d-%H%M%S"), 
            switch(input$downloadFormat, 
                   csv = ".csv", 
                   xlsx = ".xlsx", 
                   html = ".html",
                   ".csv"), sep = "")
    },
    content = function(file) {
      req(manovaResults())
      
      # Prepare results
      results_list <- list(
        "Multivariate Tests" = manovaResults()$multivariate,
        "Between-Subjects Effects" = manovaResults()$univariate
      )
      
      if (!is.null(manovaResults()$postHoc)) {
        results_list[["Post-Hoc Tests"]] <- manovaResults()$postHoc
      }
      
      results_list[["Descriptive Statistics"]] <- manovaResults()$descriptives
      
      # Export based on format
      if (input$downloadFormat == "csv" || is.null(input$downloadFormat)) {
        # Combine all results into one data frame with section headers
        all_results <- do.call(rbind, lapply(names(results_list), function(name) {
          df <- results_list[[name]]
          df$Section <- name
          return(df)
        }))
        
        write.csv(all_results, file, row.names = FALSE)
      } else if (input$downloadFormat == "xlsx") {
        if (!requireNamespace("writexl", quietly = TRUE)) {
          showNotification("writexl package is required for Excel export. Using CSV instead.", type = "warning")
          write.csv(do.call(rbind, results_list), file, row.names = FALSE)
        } else {
          writexl::write_xlsx(results_list, path = file)
        }
      } else if (input$downloadFormat == "html") {
        html_content <- c(
          "<html><head><style>",
          "body { font-family: Arial, sans-serif; margin: 20px; }",
          "h1 { color: #2c3e50; }",
          "h2 { color: #3498db; margin-top: 30px; }",
          "table { border-collapse: collapse; width: 100%; margin-bottom: 20px; }",
          "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }",
          "th { background-color: #f2f2f2; }",
          "tr:nth-child(even) { background-color: #f9f9f9; }",
          "</style></head><body>",
          "<h1>MANOVA Analysis Results</h1>"
        )
        
        for (name in names(results_list)) {
          df <- results_list[[name]]
          html_content <- c(html_content, 
                            paste0("<h2>", name, "</h2>"),
                            "<table>",
                            "<tr>", paste0("<th>", names(df), "</th>", collapse = ""), "</tr>")
          
          for (i in 1:nrow(df)) {
            html_content <- c(html_content,
                              "<tr>", 
                              paste0("<td>", as.character(df[i, ]), "</td>", collapse = ""),
                              "</tr>")
          }
          
          html_content <- c(html_content, "</table>")
        }
        
        html_content <- c(html_content, "</body></html>")
        writeLines(html_content, file)
      }
    }
  )
  
  # Update analysis status
  observeEvent(input$runAnalysis, {
    output$analysisStatus <- renderUI({
      if (!is.null(manovaResults())) {
        tags$div(class = "alert alert-success", "Analysis completed successfully!")
      } else {
        tags$div(class = "alert alert-danger", "Analysis failed. Please check your inputs.")
      }
    })
  })
}
  
  