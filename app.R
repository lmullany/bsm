# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

# source files --------------------------------------------------------------
source("src/00_setup.R")


ui <- page(
  theme = bs_theme(version = 5, preset = BOOT_PRESET),
  tags$head(tags$style(HTML("
        .shiny-output-error-validation {
          color: red;
        }
        .card {border: 0;}
      "))),
  useShinyjs(),
  page_navbar(
    title = "Bayesian Spatiotemporal Modeling",
    data_loader_ui("data_load"),
    inla_model_ui("inla_model"),
    viz_ui("viz"),
    nav_spacer(),
    nav_panel(
      "Documentation",
      uiOutput(outputId = "app_documentation")
    ),
    nav_item(input_dark_mode(mode="dark")),
    navbar_options = list(class = "bg-primary", theme = "dark", underline=FALSE)
  )
)

server <- function(input, output, session) {
  
  # ----------------------------------------------------------------------
  # Global Reactives for Profile
  # ----------------------------------------------------------------------
  profile <- reactiveVal(CREDENTIALS$profile)
  valid_profile <- reactiveVal(CREDENTIALS$valid)
  
  # ----------------------------------------------------------------------
  # Documentation
  # ----------------------------------------------------------------------
  output$app_documentation <- renderUI({
    HTML(
      markdown::markdownToHTML(
        file="src/documentation/documentation.md",
        fragment.only = TRUE
      )
    )
  })
  
}

#-----------
shinyApp(ui, server)

