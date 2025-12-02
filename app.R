# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

# source files --------------------------------------------------------------
source("src/00_setup.R")
source("src/02_custom_filters.R")

options(shiny.maxRequestSize = 100*1024^2) 
ui <- page(
  # get theme from the setup file
  theme = THEME,
  tags$head(
  tags$style(HTML("
        .shiny-output-error-validation {
          color: red;
        }
        .card {
          border: 0;
        }
        [id$='_progress'] {
          white-space: normal !important;
          height: auto !important;
          min-height: 2.2em !important;
          overflow: visible !important;
          margin-top: 0.5rem;
        }
    ")),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('clear-reactable-filters', function(message) {
      var id = message.id;
      console.log('[clear-reactable-filters] message received for id:', id);

      if (!window.Reactable) {
        console.warn('[clear-reactable-filters] window.Reactable not found');
        return;
      }

      // 1. Clear all column filters + global search
      Reactable.setAllFilters(id, []);
      try {
        Reactable.setSearch(id, undefined);
      } catch (e) {
        console.warn('Reactable.setSearch failed:', e);
      }

      // 2. Reset custom filter UI elements inside this table

      var container = document.getElementById(id);
      if (!container) {
        console.warn('[clear-reactable-filters] table container not found for id:', id);
        return;
      }

      // 2a. Clear text/number/date inputs (min/max, date ranges, etc.)
      var inputs = container.querySelectorAll(
        'input[type=text], input[type=number], input[type=date]'
      );
      inputs.forEach(function(input) {
        input.value = '';
      });

      // 2b. Reset checkboxes:
      //     - uncheck all
      //     - then re-check any with value='__ALL__' (our All-option)
      var checkboxes = container.querySelectorAll('input[type=checkbox]');
      checkboxes.forEach(function(cb) {
        cb.checked = false;
      });

      var allBoxes = container.querySelectorAll('input[type=checkbox][value=\"__ALL__\"]');
      allBoxes.forEach(function(cb) {
        cb.checked = true;
      });
    });
  "))
  ),
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
  
  dc = reactiveValues() # data loader configuration reactives
  im = reactiveValues() # inla model configuration reactives
  results = reactiveValues()  # results reactives (data, plots, model, etc)
  
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

