library(shiny)
library(ggplot2)
library(plotly)
library(shinyjs)

fluidPage(
  useShinyjs(),
  # Application title
  titlePanel("lensFX"), br(), 
  actionButton("run", "Update results and plots", class = "btn-warning"), br(), br(),

  tabsetPanel(
    id = "all_tabs",
    tabPanel(
      title = "Up- & download", 
      sidebarPanel(
        width = 8,
        fileInput("upload_all_input", "Upload input", accept = ".rds"),
        downloadButton("download_all_input", "Download input")
      )
    ),
    tabPanel(
      title = "System",
      sidebarPanel(
        width = 4,
        numericInput("Mv_NAP", "Maaiveldniveau (m+NAP)", 4.3, step = 0.01),
        numericInput("Gr_NAP", "Greppelniveau (m+NAP)", 4.0, step = 0.01),
        numericInput("Topc0_NAP", "Bovenkant c0 (m+NAP)", 0, step = 0.01),
        numericInput("EW_diepte", "Effectieve worteldiepte (m-mv)", 0.25, min=0, step = 0.01),
        numericInput("z_krit", "kritieke z-afstand (m)", 0.70, min=0, step = 0.01),
        numericInput(
          "PorSat",
          "Effective porositeit (-)",
          0.4,
          min = 0.1,
          max = 0.8,
          step = 0.01
        ),
        numericInput(
          "PorFr",
          "Freatische bergingscoeff. (-)",
          0.15,
          min = 0.01,
          max = 0.4,
          step = 0.01
        ),
        numericInput("c0", "Hydraulische weerstand c0 (d)", 50, min = 25),
        numericInput("Wdr", "Drainage weerstand (d)", 75, min = 1),
        numericInput( "inf_fact", "Infiltratiefactor (-)", value = 0, min = 0, max = 1, step = 0.1 ),
        numericInput("Cklein", "Greppelweerstand (d)", 25, min = 1),
        numericInput(
          inputId = "I_nSubStep",
          label = "Number of Substeps in Calculation",
          value = 10,
          min = 0,
          max = 100,
          step = 1
        )
      )
    ),
    tabPanel(
      title = "Initial condition",
      sidebarPanel(
        numericInput(
          inputId = "I_GWS_NAP",
          label = "Initieel niveau grondwaterstand (m+NAP)",
          value = 4,
          step = 0.01
        ),
        numericInput(
          inputId = "I_GV_NAP",
          label = "Initieel niveau van grensvlak (m+NAP)",
          value = 3.5,
          step = 0.01
        )
      )
    ),
    tabPanel(
      title = "Stresses", 
      sidebarPanel(
        width=5,
        fileInput("stresses_file", "Upload Excel file", accept = c(".xlsx", ".xls")), 
        downloadButton("download_stresses_excel", "Download stresses (Excel)"),
        br(), br(),
        actionButton("clear_stresses", "Clear stresses", class = "btn-danger")
      ),
      tableOutput("view_stresses_table")
    ),
    tabPanel(
      title = "Observations", 
      sidebarPanel(
        width = 5,
        fileInput("observations_file", "Upload Excel file", accept = c(".xlsx", ".xls")),
        downloadButton("download_observations_excel", "Download observations (Excel)"),
        br(), br(),
        actionButton("clear_observations", "Clear observations", class = "btn-danger")
      ),
      tableOutput("view_observations_table")
    ),     
    
    
    tabPanel(
      title = "optimisation",
      value = "optimisation",
      
      sidebarLayout(
        sidebarPanel(
          width = 4,
          
          h4("Parameter bounds"),
          
          sliderInput(
            inputId = "opt_c0_range",
            label   = "Hydraulic resistance c0 (days)",
            min     = 1,
            max     = 500,
            value   = c(25, 150),
            step    = 1
          ),
          
          sliderInput(
            inputId = "opt_Wdr_range",
            label   = "Drainage resistance Wdr (days)",
            min     = 1,
            max     = 500,
            value   = c(10, 200),
            step    = 1
          ),
          
          sliderInput(
            inputId = "opt_Cklein_range",
            label   = "Ditch resistance Cklein (days)",
            min     = 1,
            max     = 200,
            value   = c(5, 80),
            step    = 1
          ),
          
          hr(),
          
          h4("Optimisation settings"),
          
          sliderInput(
            "opt_weight_gws",
            "Relative weight GWS vs GV",
            min = 0, max = 1, value = 0.5, step = 0.05
          ),
          
          selectInput(
            inputId = "opt_method",
            label   = "Optimisation method (bounded)",
            choices = c(
              "L-BFGS-B (bounded, recommended)" = "L-BFGS-B"
            ),
            selected = "L-BFGS-B"
          ),
          
          numericInput(
            inputId = "opt_maxiter",
            label   = "Maximum iterations",
            value   = 200,
            min     = 10,
            max     = 5000,
            step    = 10
          ),
          
          hr(),
          
          h4("Exclude parameters from optimisation"),
          
          helpText(
            "Select parameters to keep fixed. ",
            "You may exclude up to two parameters (at least one must remain free)."
          ),
          
          checkboxGroupInput(
            inputId = "opt_exclude_params",
            label   = NULL,
            choices = c(
              "c0 (hydraulic resistance)"   = "c0",
              "Wdr (drainage resistance)"   = "Wdr",
              "Cklein (ditch resistance)"   = "Cklein"
            ),
            selected = character(0)
          ),
          
          div(
            style = "font-size: 0.9em; color: #666;",
            "Example: selecting c0 and Wdr will optimise only Cklein."
          ),
          
          actionButton(
            inputId = "run_optimisation",
            label   = "Run optimisation",
            class   = "btn-danger"
          )
          
        ),
        
        mainPanel(
          width = 8,
          helpText(
            "The optimisation minimises the mismatch between observed and simulated ",
            "groundwater levels (GWS and GV) within the specified parameter bounds."
          ), br(), br(),
          # ---------------------------
          # OPTIMISATION STATUS
          # ---------------------------
          h4("Optimisation status"),
          verbatimTextOutput("opt_status"),
          
          br(),
          
          # ---------------------------
          # OPTIMISATION SUMMARY
          # ---------------------------
          #h4("Optimisation summary"),
          #tableOutput("opt_summary"),
          
          #br(),
          
          # ---------------------------
          # OPTIMISED PARAMETERS
          # ---------------------------
          h4("Optimised parameters"),
          tableOutput("opt_results"),
          #br(),
          
          
          #h4("Parameter sensitivity"),
          #tableOutput("sensitivity_table"),
          
          br(),
          
          h4("Scatterplots"),
          fluidRow(
            column(
              6,
              h5("Before optimisation"),
              plotlyOutput("plot_xy_before", height = "360px")
            ),
            column(
              6,
              h5("After optimisation"),
              plotlyOutput("plot_xy_after", height = "360px")
            )
          )
        )       
        
      )
    ),      
    
    tabPanel(
      title = "Results", 
      sidebarPanel(
        width = 3,
        
        downloadButton("download_stats_excel", "Download statistics table (Excel)"),
        br(), br(),
        downloadButton("download_results_excel", "Download results table (Excel)"), br(), br(),
        
        dateInput("stats_from", "Statistieken van:", value = NULL,
                  format = "dd-mm-yyyy", language = "nl"),
        dateInput("stats_to", "Statistieken tot:", value = NULL,
                  format = "dd-mm-yyyy", language = "nl"),        
        tableOutput("stats_table")
      ),
      tableOutput("view_results_table"),
    ),
    tabPanel(title = "Plots",
             tabsetPanel(
               tabPanel(
                 title = "Levels",
                 plotlyOutput("plot_GWS_GV", height = "720px")
               ),
               tabPanel(title = "Fluxes", sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   checkboxGroupInput(
                     inputId = "ts_select",
                     label   = "Select time serie(s):",
                     choices = c("NN", "OA", "Kwel", "Dr", "DrB", "DrA"),
                     selected = c("NN", "Dr", "Kwel")
                   )
                 ),
                 mainPanel(width = 9, plotlyOutput("plot_fluxes", height = "720px"))
               )), 
               tabPanel(
                 title = "Volumes",
                 plotlyOutput("plot_A_B", height = "720px")
              ),
               tabPanel(
                 title = "Volumes (cumulative)",
                 plotlyOutput("plot_cum", height = "720px")
               )
             )), 
    
    tabPanel(title = "Documentation",
             tabsetPanel(
               tabPanel(title = "Schematic Diagram", tags$div(
                 img(src = "schema neerslaglenzen model.png", width =
                       "600px")
               )),
               tabPanel(title = "Symbols", tags$div(
                 img(src = "symbolen neerslaglenzen model.png", width =
                       "600px")
               )),
               tabPanel(title = "Formulary", tags$div(
                 img(src = "formularium neerslaglenzen model.png", width =
                       "600px")
               )),
               tabPanel(
                 title = "Technical background",
                 tags$ul(tags$li(
                   tags$a(
                     href = "Waterbalansmodel documentatie mei 1994.pdf",
                     "Technische documentatie neerslaglenzen model(PDF)",
                     target = "_blank"
                   )
                 ), tags$li(
                   tags$a(href = "https://github.com/KeesVanImmerzeel/lensFX", "lensFX op GitHub")
                 ))
               ), 
               tabPanel(
                 title = "Articles",
                 
                 h4("Download Articles"),
                 
                 tags$ul(
                   tags$li(
                     tags$a(
                       href = "Toepassing neerslaglenzen model de Zumpe.pdf",
                       "Toepassing van een neerslaglenzenmodel bij hydro-ecologische herstelprojecten (PDF)",
                       target = "_blank"
                     )
                   ),
                   tags$li(
                     tags$a(
                       href = "neerslaglenzen_vorm_en_dynamiek-hydrotheek_(stowa)_10059.pdf",
                       "Neerslaglenzen: vorm en dynamiek (PDF)",
                       target = "_blank"
                     )
                   ),
                   tags$li(
                     tags$a(
                       href = "SWI_99.157_Neerslaglenzen_in_kwelgebieden.pdf",
                       "Modellering van neerslaglenzen in kwelgebieden (PDF)",
                       target = "_blank"
                     )
                   ),
                   tags$li(
                     tags$a(
                       href = "2001-4_kwelvensters_onder_gebouwen_en_in_het_vrije_veld.pdf",
                       "Kwelvensters onder gebouwen en in het vrije veld (PDF)",
                       target = "_blank"
                     )
                   ),
                   tags$li(
                     tags$a(
                       href = "Regenwaterlenzen stowa.pdf",
                       "Deltafact Stowa: Regenwaterlenzen (PDF)",
                       target = "_blank"
                     )
                   ),
                   tags$li(
                     tags$a(
                       href = "berekenen van de kritieke z-afstanden en verzadigingstekorten.pdf",
                       "Kritieke z-afstanden, bewortelingsdiepte (PDF)",
                       target = "_blank"
                     )
                   )                   
                 )
               )
             ))
  ),
  tags$script(HTML("
  Shiny.addCustomMessageHandler('click', function(id) {
    document.getElementById(id).click();
  });
"))

)








