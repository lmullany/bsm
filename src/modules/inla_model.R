inla_model_ui <- function(id) {
  btn_class <- "btn-primary btn-sm"
  ns <- NS(id)
  
  ########################
  # Input Widgets
  ########################
  
  # Number of forecasts
  forecasts = numericInput(
    ns("nforecasts"),
    label = "Number of Forecasts (weeks)",
    value = 4
  )
  
  family = selectInput(
    ns("dist_family"),
    label="Distributional Family",
    choices = c(
      "Binomial" = "binomial",
      "Negative Binomial"="nbinomial",
      "Poisson" = "poisson"
    ),
    selected = "binomial"
  )
  
  hyper_params = tagList(
    numericInput(ns("param"), "INLA Hyperparam: param", value=0.2),
    numericInput(ns("alpha"), "INLA Hyperparam: alpha", value=0.01),
  )
  formula_panel = tagList(
    radioButtons(
      ns("formula_type"),
      "Formula",
      choices = c("Default", "Custom"), 
      selected = "Default"
    ),
    conditionalPanel(
      condition = "input.formula_type == 'Custom'",
      textAreaInput(
        ns("custom_formula"),
        "Custom Formula"
      ),
      ns = ns
    )
  )
  
  
  
  ########################
  # Nav Panel to Return
  ########################
  
  nav_panel(
    title = "INLA Model",
    layout_sidebar(
      sidebar = sidebar(
        id = ns("model_sidebar"),
        width = SIDEBAR_WIDTH,
        forecasts, 
        family,
        hyper_params,
        formula_panel,
        input_task_button(ns("estimate_model_btn"), "Run Model")
      ),
      card(
        card_header("Place Holder for Model Result", class="bg-primary"),
      )
    )
  )  
}

inla_model_server <- function(id, dc, im, results) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # observe the time_res and update the forecasts label and 
      observe({
        req(dc$time_res)
        lv = update_n_forecast_widget(dc$time_res)
        updateNumericInput(
          inputId =  "nforecasts", 
          label = lv[["label"]],
          value = lv[["value"]]
        )
      })
    }
  )
}

# Helper function for forecast label and default
update_n_forecast_widget <- function(res) {
  lu = list(
    "daily" = list(label = "Number of forecasts (days)", value = 28),
    "weekly" = list(label = "Number of forecasts (weeks)", value = 4),
    "monthly" = list(label = "Number of forecasts (months)", value = 1),
    "yearly" = list(label = "Number of forecasts (years)", value=1)
  )
  
  lu[[res]]
}
