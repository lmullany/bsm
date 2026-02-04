# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

# source files --------------------------------------------------------------
source("src/00_setup.R")


options(shiny.maxRequestSize = 1000*1024^2) 
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
    nav_item(
      div(
        class = "mode-pill-wrap",
        span(class = "mode-side left",
             span(class = "mode-text", "Light"),
             span(class = "mode-icon", bsicons::bs_icon("sun-fill"))
        ),
        tags$button(
          type = "button",
          class = "mode-switch",
          onclick = "
        const html = document.documentElement;
        const cur = html.getAttribute('data-bs-theme') || 'dark';
        const next = (cur === 'dark') ? 'light' : 'dark';
        html.setAttribute('data-bs-theme', next);
        return false;
      "
        ),
        span(class = "mode-side right",
             span(class = 'mode-icon', bsicons::bs_icon('moon-stars-fill')),
             span(class = 'mode-text', 'Dark')
        )
      )
    ), 
    nav_item(tooltip_ui("tooltip")),
    # Options
    navbar_options = list(class = "card-header-accent top-dark-nav", theme = "dark", underline=FALSE)
  )
)

server <- function(input, output, session) {

  # ----------------------------------------------------------------------
  # Global Reactives for Profile
  # ----------------------------------------------------------------------
  profile <- reactiveVal(CREDENTIALS$profile)
  valid_profile <- reactiveVal(CREDENTIALS$valid)
  
  observe({
    if(!valid_profile() && ALLOW_SHINY_CREDENTIALS == TRUE)
      credServer("creds", profile, valid_profile)
  })
  
  # ----------------------------------------------------------------------
  # Global Reactives for configuration and results
  # ----------------------------------------------------------------------
  
  # data loader configuration reactives
  dc <- reactiveValues(
    physical_adj = NULL, mobility_adj = NULL,
    time_res = NULL, geo_res=NULL, drange=NULL, synd_cat=NULL, synd_drop_menu=NULL,
    states = NULL, selected_counties = NULL, includes_alaska_hawaii = NULL
  )
  
  # use this to transfer cache widget values from load query or load model
  # through modules
  cache_transitions <- reactiveValues(
    states=NULL, selected_counties=NULL, geo_res=NULL, time_res=NULL, drange=NULL,
    synd_cat=NULL, synd_drop_menu=NULL
  )
  
  # inla model configuration reactives
  im <- reactiveValues(
    model = NULL, data_cls = NULL, posterior=NULL, nforecasts=NULL
  ) 
  
  # results reactives (data, plotsm etc; add to as needed for future reporting)
  results <- reactiveValues(
    data = NULL
  )  
  
  # ----------------------------------------------------------------------
  # Module Server calls
  # ----------------------------------------------------------------------
  data_loader_server(id = "data_load", dc, results, profile, valid_profile, cache_transitions)
  inla_model_server(id = "inla_model", dc, im, results, cache_transitions)
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

