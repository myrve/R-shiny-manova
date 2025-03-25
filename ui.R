ui <- dashboardPage(
  dashboardHeader(title = "MANOVA Analysis Application"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "data", icon = icon("upload")),
      menuItem("Variable Selection", tabName = "variables", icon = icon("list")),
      menuItem("Interactions", tabName = "interactions", icon = icon("project-diagram")),
      menuItem("MANOVA Analysis", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Results", tabName = "results", icon = icon("table")),
      menuItem("Help", tabName = "help", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .tooltip-inner {
          max-width: 300px;
          text-align: left;
        }
        .interaction-item {
          margin-bottom: 10px;
          padding: 8px;
          border-radius: 4px;
          background-color: #f8f9fa;
        }
        .interaction-item.selected {
          background-color: #d4edda;
          border: 1px solid #c3e6cb;
        }
        .help-section {
          margin-bottom: 20px;
          padding: 15px;
          background-color: #f8f9fa;
          border-radius: 5px;
        }
        .help-section h4 {
          color: #007bff;
          margin-top: 0;
        }
      "))
    ),
    tabItems(
      # Data Upload Tab
      tabItem(tabName = "data",
              fluidRow(
                box(width = 12, title = "Data Upload",
                    fileInput("dataFile", "Upload Excel File", accept = c(".xlsx", ".xls")),
                    helpText("Upload your data in Excel format. The application will automatically detect variable types."),
                    tags$div(class = "alert alert-info",
                             icon("info-circle"), 
                             "Make sure your data is properly formatted with variable names in the first row."),
                    hr(),
                    h4("Data Preview"),
                    DT::dataTableOutput("dataPreview")
                )
              )
      ),
      
      # Variable Selection Tab
      tabItem(tabName = "variables",
              fluidRow(
                box(width = 6, title = "Categorical Variables (Factors)",
                    selectInput("factorVars", "Select Factors:", choices = NULL, multiple = TRUE),
                    helpText("Select categorical variables to use as factors in the MANOVA analysis."),
                    tags$div(class = "alert alert-info",
                             tags$span(
                               icon("info-circle"), 
                               "Factors are categorical variables that define groups for comparison.",
                               tags$br(),
                               "Examples: Gender, Treatment Group, Education Level"
                             )
                    )
                ),
                box(width = 6, title = "Numeric Variables (Dependent Variables)",
                    selectInput("dependentVars", "Select Dependent Variables:", choices = NULL, multiple = TRUE),
                    helpText("Select numeric variables to analyze as dependent variables."),
                    tags$div(class = "alert alert-info",
                             tags$span(
                               icon("info-circle"), 
                               "Dependent variables are continuous measures that you want to compare across groups.",
                               tags$br(),
                               "Examples: Test Scores, Blood Pressure, Income"
                             )
                    )
                )
              ),
              fluidRow(
                box(width = 12, title = "Variable Type Detection",
                    checkboxInput("advancedDetection", "Use Advanced Variable Type Detection", value = TRUE),
                    helpText("Advanced detection identifies numeric variables with few unique values as potential factors."),
                    actionButton("refreshVarTypes", "Refresh Variable Types", icon = icon("sync"))
                )
              )
      ),
      
      # Interactions Tab
      tabItem(tabName = "interactions",
              fluidRow(
                box(width = 12, title = "Factor Interactions",
                    helpText("Below are all possible interactions between the selected factors."),
                    helpText("Select which interactions to include in the MANOVA analysis."),
                    tags$div(class = "alert alert-info",
                             tags$span(
                               icon("info-circle"), 
                               "Interactions examine how the effect of one factor depends on the level of another factor.",
                               tags$br(),
                               "For example, 'Gender * Treatment' tests if the treatment effect differs between genders."
                             )
                    ),
                    actionButton("selectAllInteractions", "Select All", class = "btn-info"),
                    actionButton("deselectAllInteractions", "Deselect All", class = "btn-secondary"),
                    hr(),
                    uiOutput("interactionsUI"),
                    conditionalPanel(
                      condition = "input.factorVars && input.factorVars.length < 2",
                      tags$div(class = "alert alert-warning",
                               icon("exclamation-triangle"),
                               "Please select at least two factors to generate interactions.")
                    )
                )
              )
      ),
      
      # Analysis Tab
      tabItem(tabName = "analysis",
              fluidRow(
                box(width = 12, title = "MANOVA Analysis Configuration",
                    helpText("Configure your MANOVA analysis and run it with the selected variables and interactions."),
                    fluidRow(
                      column(6,
                             checkboxInput("includePostHoc", "Include Post-Hoc Tests", value = TRUE),
                             selectInput("postHocCorrection", "Post-Hoc Correction Method:",
                                         choices = c("Tukey" = "tukey", 
                                                     "Bonferroni" = "bonferroni", 
                                                     "Holm" = "holm", 
                                                     "None" = "none"),
                                         selected = "tukey"),
                             tags$div(class = "alert alert-info",
                                      tags$span(
                                        icon("info-circle"), 
                                        "Post-hoc tests perform pairwise comparisons between group levels.",
                                        tags$br(),
                                        "Correction methods adjust p-values to control for multiple comparisons."
                                      )
                             )
                      ),
                      column(6,
                             checkboxInput("includeDescriptives", "Include Descriptive Statistics", value = TRUE),
                             checkboxInput("assumptionChecks", "Include Assumption Checks", value = TRUE),
                             tags$div(class = "alert alert-info",
                                      tags$span(
                                        icon("info-circle"), 
                                        "Descriptive statistics provide means, standard deviations, and sample sizes.",
                                        tags$br(),
                                        "Assumption checks help verify if your data meets MANOVA requirements."
                                      )
                             )
                      )
                    ),
                    hr(),
                    actionButton("runAnalysis", "Run Analysis", class = "btn-primary btn-lg"),
                    tags$div(id = "analysisStatus")
                )
              ),
              fluidRow(
                box(width = 12, title = "Analysis Summary",
                    verbatimTextOutput("analysisSummary")
                )
              )
      ),
      
      # Results Tab
      tabItem(tabName = "results",
              fluidRow(
                box(width = 12, title = "MANOVA Results",
                    tabsetPanel(
                      tabPanel("Multivariate Tests", 
                               helpText("Multivariate test results for the MANOVA analysis."),
                               tags$div(class = "alert alert-info",
                                        tags$span(
                                          icon("info-circle"), 
                                          "Pillai's Trace, Wilks' Lambda, Hotelling's Trace, and Roy's Largest Root are different statistics to test the significance of MANOVA effects. Pillai's Trace is most robust to violations of assumptions."
                                        )
                               ),
                               DT::dataTableOutput("multivariateTests")),
                      tabPanel("Between-Subjects Effects", 
                               helpText("Tests of between-subjects effects for each dependent variable."),
                               tags$div(class = "alert alert-info",
                                        tags$span(
                                          icon("info-circle"), 
                                          "These tests examine the effect of each factor and interaction on individual dependent variables, similar to separate ANOVAs."
                                        )
                               ),
                               DT::dataTableOutput("betweenSubjectsEffects")),
                      tabPanel("Post-Hoc Tests", 
                               helpText("Post-hoc test results for pairwise comparisons."),
                               tags$div(class = "alert alert-info",
                                        tags$span(
                                          icon("info-circle"), 
                                          "Post-hoc tests compare each level of a factor to determine which specific groups differ significantly from each other."
                                        )
                               ),
                               DT::dataTableOutput("postHocTests")),
                      tabPanel("Descriptive Statistics", 
                               helpText("Descriptive statistics for the dependent variables."),
                               tags$div(class = "alert alert-info",
                                        tags$span(
                                          icon("info-circle"), 
                                          "These statistics show means, standard deviations, and sample sizes for each group and dependent variable."
                                        )
                               ),
                               DT::dataTableOutput("descriptiveStats"))
                    ),
                    hr(),
                    fluidRow(
                      column(6,
                             selectInput("downloadFormat", "Select Format:",
                                         choices = c("CSV" = "csv", "Excel" = "xlsx", "HTML" = "html"),
                                         selected = "csv")
                      ),
                      column(6,
                             downloadButton("downloadResults", "Download Results", class = "btn-success"),
                             helpText("Download the analysis results in the selected format.")
                      )
                    )
                )
              )
      ),
      
      # Help Tab
      tabItem(tabName = "help",
              fluidRow(
                box(width = 12, title = "MANOVA Analysis Help",
                    tags$div(class = "help-section",
                             h4("What is MANOVA?"),
                             p("Multivariate Analysis of Variance (MANOVA) is an extension of ANOVA that allows you to analyze multiple dependent variables simultaneously while controlling for the experiment-wise error rate."),
                             p("MANOVA is appropriate when you have two or more dependent variables that are conceptually related and correlated with each other.")
                    ),
                    tags$div(class = "help-section",
                             h4("When to Use MANOVA?"),
                             p("Use MANOVA when:"),
                             tags$ul(
                               tags$li("You have multiple dependent variables that are related"),
                               tags$li("You want to examine how categorical independent variables (factors) affect the dependent variables"),
                               tags$li("You want to test for interaction effects between factors"),
                               tags$li("You want to control the overall Type I error rate")
                             )
                    ),
                    tags$div(class = "help-section",
                             h4("Understanding Interactions"),
                             p("An interaction occurs when the effect of one factor depends on the level of another factor."),
                             p("For example, a 'Gender * Treatment' interaction would indicate that the treatment effect differs between males and females."),
                             p("This application allows you to select which specific interactions to include in your analysis.")
                    ),
                    tags$div(class = "help-section",
                             h4("Interpreting Results"),
                             p("MANOVA results include:"),
                             tags$ul(
                               tags$li(strong("Multivariate Tests:"), " These test the overall effect of each factor and interaction across all dependent variables simultaneously."),
                               tags$li(strong("Between-Subjects Effects:"), " These are similar to individual ANOVAs for each dependent variable."),
                               tags$li(strong("Post-Hoc Tests:"), " These compare specific group levels to identify which groups differ significantly."),
                               tags$li(strong("Descriptive Statistics:"), " These provide means, standard deviations, and sample sizes for each group.")
                             )
                    ),
                    tags$div(class = "help-section",
                             h4("MANOVA Assumptions"),
                             p("MANOVA has several assumptions:"),
                             tags$ul(
                               tags$li("Multivariate normality of dependent variables"),
                               tags$li("Homogeneity of variance-covariance matrices across groups"),
                               tags$li("Independence of observations"),
                               tags$li("No extreme multivariate outliers"),
                               tags$li("Linear relationships among dependent variables"),
                               tags$li("No multicollinearity or singularity")
                             )
                    )
                )
              )
      )
    )
  )
)
