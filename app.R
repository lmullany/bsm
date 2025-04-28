# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

# source files --------------------------------------------------------------
source("src/00_setup.R")


ui <- page(
  # get theme from the setup file
  theme = THEME,
  tags$head(tags$style(HTML("
        .shiny-output-error-validation {
          color: red;
        }
        .card {border: 0;}
      "))),
  useShinyjs(),
  page_navbar(
    title = "Bayesian Spatiotemporal Modeling",
    # Data Loading UI
    data_loader_ui("data_load"),
    # INLA Estimation UI
    inla_model_ui("inla_model"),
    # VIZ UI
    viz_ui("viz"),
    # SPACER
    nav_spacer(),
    # Documentation UI
    documentation_ui("documentation"),
    # Dark Mode Toggle
    nav_item(input_dark_mode(mode="dark")),
    # Options
    navbar_options = list(class = "b-primary", theme = "dark", underline=FALSE)
  )
)

server <- function(input, output, session) {
  
  # ----------------------------------------------------------------------
  # Global Reactives for Profile
  # ----------------------------------------------------------------------
  profile <- reactiveVal(CREDENTIALS$profile)
  valid_profile <- reactiveVal(CREDENTIALS$valid)
  
  # ----------------------------------------------------------------------
  # Global Reactives for configuration and results
  # ----------------------------------------------------------------------
  
  dc = reactiveValues() # data loader configuration reactives
  im = reactiveValues() # inla model configuration reactives
  #results = reactiveValues(data=data.table::fread("~/../Downloads/demo_inla_data_md.csv"))  # results reactives (data, plots, model, etc)
  results = reactiveValues()  # results reactives (data, plots, model, etc)
  
  # ----------------------------------------------------------------------
  # Module Server calls
  # ----------------------------------------------------------------------
  data_loader_server(id = "data_load", dc, results, profile)
  inla_model_server(id = "inla_model", dc, im, results)
  viz_server("viz", dc, im, results)
  documentation_server(id="documentation")
  
}

#-----------
shinyApp(ui, server)

