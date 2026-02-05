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
    id = "main_nav",
    title = "Bayesian Spatiotemporal Modeling",
    # Data Loader
    data_loader_ui("data_load"),
    # INLA Modeling
    inla_model_ui("inla_model"),
    # VIZ Ui
    viz_ui("viz"),
    # Spacer
    nav_spacer(),
    # Extras
    nav_item(
      bslib::popover(
        tags$button(
          type = "button",
          class = "btn btn-link nav-link p-0",
          bsicons::bs_icon("gear", title = "Extras")
        ),
        div(class = "d-grid gap-2",
            
            # Button with book icon that will open the modal for documentation
            actionButton(
              "open_docs",
              "Documentation",
              icon = bsicons::bs_icon("book"),class=BUTTON_CLASS
            ),
            
            # Tool tip ui for toggling on off
            tooltip_ui("tooltip"),
            
            # Div with the light/dark toggle switch
            div(class = "btn btn-primary btn-sm settings-toggle-row",
                role = "button",
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
                     span(class = "mode-icon", bsicons::bs_icon("moon-stars-fill")),
                     span(class = "mode-text", "Dark")
                )
              )
            )
        ),
        id = "settings_pop",
        placement = "bottom",
        options = list(close_button = TRUE)
      )
    ),
    
    navbar_options = list(class = "card-header-accent top-dark-nav", theme = "dark", underline = FALSE)
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
  docs_body <- documentation_server(id="documentation")
  
  # ----------------------------------------------------------------------
  # Other functionality
  # ----------------------------------------------------------------------
  
  # Hide the main viz panel until we have a model
  observe(toggleState(
    condition = !is.null(im$model), selector = 'a[data-value="viz-viz_main"]'
  ))

  observe({
    shiny::showModal(
      shiny::modalDialog(
        title = "",
        div(style = "max-height: 70vh; overflow-y: auto; padding-right: 0.5rem;", docs_body),
        size = "l",
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      )
    )
    bslib::toggle_popover("settings_pop", show = FALSE)
  }) |> bindEvent(input$open_docs)
}


#-----------
shinyApp(ui, server)

