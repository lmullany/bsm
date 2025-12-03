# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

# source files --------------------------------------------------------------
source("src/00_setup.R")


options(shiny.maxRequestSize = 100*1024^2) 
ui <- page(
  # get theme from the setup file
  theme = THEME,
  global_ui_tags,
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
    # Dark Mode and Tooltip Toggles
    nav_item(input_dark_mode(mode="dark")), nav_item(tooltip_ui("tooltip")),
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
  
  # data loader configuration reactives
  dc = reactiveValues(
    physical_adj = NULL, mobility_adj = NULL,
    time_res = NULL, geo_res=NULL, drange=NULL, synd_cat=NULL, synd_drop_menu=NULL,
    states = NULL, selected_counties = NULL, includes_alaska_hawaii = NULL
  )
  
  # inla model configuration reactives
  im = reactiveValues(
    model = NULL, data_cls = NULL, posterior=NULL, nforecasts=NULL
  ) 
  
  # results reactives (data, plotsm etc; add to as needed for future reporting)
  results = reactiveValues(
    data = NULL
  )  
  
  # ----------------------------------------------------------------------
  # Module Server calls
  # ----------------------------------------------------------------------
  data_loader_server(id = "data_load", dc, results, profile)
  inla_model_server(id = "inla_model", dc, im, results)
  viz_server("viz", dc, im, results)
  tooltip_server("tooltip")
  documentation_server(id="documentation")
  
  # ----------------------------------------------------------------------
  # Other functionality
  # ----------------------------------------------------------------------
  
  # Hide the main viz panel until we have a model
  observe(toggleState(
    condition = !is.null(im$model), selector = 'a[data-value="viz-viz_main"]'
  ))
  
  
}


#-----------
shinyApp(ui, server)

