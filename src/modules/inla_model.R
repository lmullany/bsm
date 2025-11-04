# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958


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
  
  hidden(region_re_options <- tagList(
    div(
      class = "well",
      HTML("Precision Hyper-parameters:"),
      numericInput(ns("rre_prec_pc_param"), "PC Prior Sigma Threshold", value=.2, step = .001),
      sliderInput(ns("rre_prec_pc_alpha"), "PC Prior Probability", value=0.01, min=1e-5, max = 1)
    )
  ))
  
  spatial_component_options <- tagList(
    radioButtons(
      ns("sco_adjacency_type"), "Neighborhood Basis", 
      choices=c(
        "Mobility" = "mobility_adj_mat",
        "Distance" = "distance_adj_mat"
      ),
      inline=TRUE
    ),
    selectInput(
      ns("sco_model_type"), "Spatial Model",
      choices = c(
        "Proper Besag Model" = "besagproper",
        "Besag Area Model" = "besag",
        "Besag-York-Mollier" = "bym"
      )
    ),
    selectInput(
      ns("sco_control_group_model"), "Temporal Model",
      choices = c(
        "Autoregressive" = "ar",
        "Random Walk (Order 2)" = "rw2",
        "Random Walk (Order 1)" = "rw1"
      )
    ),
    conditionalPanel(
      condition = "input.sco_control_group_model == 'ar'",
      numericInput(ns("sco_control_group_ar_order"), "Order", value = 1, min=1, max=5), 
      ns=ns
    ),
    div(
      class = "well",
      HTML("Precision Hyper-parameters:"),
      numericInput(ns("sco_prec_pc_param"), "PC Prior Sigma Threshold", value=.2, step = .001),
      numericInput(ns("sco_prec_pc_alpha"), "PC Prior Probability", value=0.01, min=0, max = 1, step=0.01)
    )
  )
  
  temporal_component_options <- tagList(
    selectInput(
      ns("tco_model"), "Model",
      choices = c(
        "Random Walk (Order 2)-Cyclical" = "rw2",
        "Random Walk (Order 1)-Cyclical" = "rw1",
        "Autoregressive-Cyclical" = "ar1",
        "Autoregressive - Temporal" = "ar"
      ),
      selected = "rw2"
    ),
    conditionalPanel(
      condition = "input.tco_model == 'ar'",
      numericInput(ns("tco_model_ar_order"), "Order", value = 1, min=1, max=5), 
      ns=ns
    )
  )
  
  # Custom model panel
  model_component_custom_panel = tagList(
    div(
      class="well", 
      layout_columns(
        checkboxInput(ns("rre_component_chkbx"), "Region Random Effect",value = TRUE),
        hidden(input_switch(ns("customize_rre"),label ="Advanced Customization",value = FALSE)),
        col_widths = c(6,6)
      ),
      conditionalPanel(
        condition = "input.customize_rre",
        region_re_options,
        ns=ns
      )
    ),
    div(
      class = "well",
      layout_columns(
        checkboxInput(ns("spatial_component_chkbx"), "Spatio-Temporal Component",value = FALSE),
        input_switch(ns("customize_spatial_component"),label ="Advanced Customization",value = FALSE),
        col_widths = c(6,6)
      ),
      conditionalPanel(
        condition = "input.customize_spatial_component",
        spatial_component_options, 
        ns=ns
      )
    ),
    div(
      class="well",
      layout_columns(
        checkboxInput(ns("temporal_component_chkbx"), "Seasonal/Temporal Component",value = FALSE),
        input_switch(ns("customize_temporal_component"),label ="Advanced Customization",value = FALSE),
        col_widths = c(6,6)
      ), 
      conditionalPanel(
        condition = "input.customize_temporal_component",
        temporal_component_options,
        ns=ns
      )
    )
  )
  
  #hyper_params = tagList(
  #  numericInput(ns("param"), "INLA Hyperparam: param", value=0.2),
  #  numericInput(ns("alpha"), "INLA Hyperparam: alpha", value=0.01),
  #)
  
  formula_panel = tagList(
    radioButtons(
      ns("formula_type"),
      "Estimation Model",
      choices = c(
        "Default Model"="default",
        "Customize Components" = "custom_components"
        #"Custom Model Formula" = "custom_formula"
      ), 
      selected = "default",
      inline = TRUE
    ),
    tags$details(
      tags$summary("Show Generic Formula"), 
      verbatimTextOutput(ns("inla_model_formula_r")) |> 
        tagAppendAttributes(style = css("white-space" = "pre-wrap"))
    ),
    conditionalPanel(
      condition = "input.formula_type == 'custom_components'",
      model_component_custom_panel,
      ns = ns
    ),
    conditionalPanel(
      condition = "input.formula_type == 'custom_formula'",
      textAreaInput(ns("custom_formula"), "Enter model formula"),
      ns = ns
    )
  )

  
  ## Output cards:
  model_card <- card(
    card_header("INLA Estimation Summary", class="bg-primary"),
    card_body(withSpinner(
      verbatimTextOutput(ns("inla_model_object")),
      caption = "Estimating Model ... please wait",
      color = bs_get_variables(theme=THEME,"primary")
    ))
  )
  
  model_data_card <- card(
    card_header("Processed Data", class="bg-primary"),
    card_body(withSpinner(
      DTOutput(ns("inla_model_data")),
      caption = "Estimating Model ... please wait",
      color = bs_get_variables(theme=THEME,"primary")
    )),
    card_footer(
      downloadButton(
        ns("download_data"),
        label="Download Data", 
        class="btn-primary"
      )
    )
  )
  
  model_formula_card <- card(
    card_header("Model/Formula", class="bg-primary"),
    card_body(withSpinner(
      verbatimTextOutput(ns("inla_model_formula")) |>
        tagAppendAttributes(style = css("white-space" = "pre-wrap")),
      caption = "Estimating Model ... please wait",
      color = bs_get_variables(theme=THEME,"primary")
    ))
  )


  
  
  ########################
  # Nav Panel to Return
  ########################
  
  nav_panel(
    title = "INLA Model",
    layout_sidebar(
      sidebar = sidebar(
        id = ns("model_sidebar"),
        width = SIDEBAR_WIDTH*2,
        forecasts, 
        family,
        formula_panel,
        input_task_button(ns("estimate_model_btn"), "Run Model")
      ),
      layout_column_wrap(
        width=NULL, height=300, 
        style = css(grid_template_columns = c("60%", "40%")),
        model_data_card,
        model_card
      ),
      wellPanel(
        downloadButton(
          ns("model_output"),
          label = "Download Model Outputs (.rds)",
          class="btn-primary btn-sm"
        ),
        actionButton(ns("actual_formula"), "Show Actual Formula", class = "btn-primary btn-sm")
      )
    )
  )  
}

inla_model_server <- function(id, dc, im, results) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # update global reactive model object
      observe(im$model <- inla_model()$model)
      # update global reactive data_class object for current model result
      observe(im$data_cls <- inla_model()$data_class)
      # update global reactive posteriors for current model
      observe(im$posterior <- add_posteriors(
        data_cls = inla_model()$data_class,
        model = inla_model()$model
        )) |> bindEvent(inla_model())
      observe(im$nforecasts <- input$nforecasts)
      
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
    
      formula_r <- reactive(
        get_formula(
          formula_type = input$formula_type,
          input = setNames(
            lapply(names(input), \(n) input[[n]]),
            names(input)
          )
        )
      )
      
      output$inla_model_formula_r <- renderPrint({
        deparse1(formula_r()) |> cat()
      })
      
      # On click of "Estimate Model", we will want to
      # 1. Validate the inputs
      # 2. Pre-process transform the data
      # 3. get adjacency matrice(s)
      # 4. Create the formula (custom or default)
      # 5. Run the model
  
      inla_model <- reactive({
        
        # We must have data, or we can't estimate model
        validate(need(results$data, "Please load data first"))
        
        # If customize components, we should at least have one
        # of spatial or temporal checked
        if(input$formula_type == "custom_components") {
          validate(need(
            input$spatial_component_chkbx || input$temporal_component_chkbx,
            "Include either spatial or temporal, or both"
          ))
        } 
  
        
        #1. TODO: VALIDATE INPUTS
        
        #2. Preprocess Data:
        data_cls <- pre_process_data(results$data, input$nforecasts)
        
        #3 Set adjacency matrix
        # Note that we set it here, but we really should set to NULL
        # and pass that NULL to the fit_model function if not needed
        if(input$formula_type == "default") {
          adj_mat_raw <- read_mobility_adj_mat()
        } else if (input$spatial_component_chkbx == TRUE) {
          if(input$sco_adjacency_type == "mobility_adj_mat") {
            adj_mat_raw <- read_mobility_adj_mat()
          } else {
            adj_mat_raw <- read_physical_adj_mat()
          }
        } else {
          adj_mat_raw <- NULL
        }        
        
        #4 Create the formula, with generic region and date ids
        formula <- get_formula(
          formula_type = input$formula_type,
          input = setNames(
            lapply(names(input), \(n) input[[n]]),
            names(input)
          )
        )
        formula = eval(formula)
        
        #5. fit the model
        model <- epistemic::fit_model(
          data_cls=data_cls,
          formula=formula,
          family = input$dist_family,
          reformulate = TRUE,
          adjacency_matrix = adj_mat_raw
        )
                
        return(list(
          model = model$inla_model,
          data_class = model$data,          
          formula = deparse1(model$formula)
        ))
        

      }) |> bindEvent(input$estimate_model_btn)
    
    
      output$inla_model_object <- renderPrint({
        req(inla_model())
        summary(inla_model()$model)
      })

      output$inla_model_formula <- renderPrint({
        req(inla_model())
        inla_model()$formula |> cat()
      })
      
      output$inla_model_data <- renderDT({
        datatable(
          inla_model()$data_class$data,
          rownames=FALSE
        )
      })
      
      output$download_data <- downloadHandler(
        filename = "processed_data.csv" ,
        content = \(file) data.table::fwrite(inla_model()$data_class$data, file)
      )
      
      output$model_output <- downloadHandler(
        filename = "model_outputs.rds" ,
        content = \(file) saveRDS(inla_model(), file)
      )
      
      # show modal box if actual formula button is pressed
      observe({
        
        req(inla_model())
        
        showModal( 
          modalDialog( 
            title = "Actual Formula Ingested by INLA",
            easyClose = TRUE,
            size = "l", 
            card(div(inla_model()$formula, style="font-size:80%"))
          ) 
        ) 
      }) |> bindEvent(input$actual_formula)  
      
      
      
    }
  )
}

add_posteriors <- function(data_cls, model){
  medians <- epistemic::get_posterior_medians(
    inla_model = model,
    data_cls = data_cls,
    use_count_scale = TRUE
  )
  cis = epistemic::get_credible_intervals(
    inla_model = model,
    data_cls = data_cls,
    ci_width = 0.95,
    use_count_scale = TRUE
  )
  merged <- merge(
    medians,
    cis,
    by=c(data_cls$region_column,data_cls$date_column),
    all=TRUE
  )
  
  rename_map <- c("0.5quant_median_counts" = "predicted_median", "counts_0.025" = "predicted_lower", "counts_0.975"="predicted_upper") 
  existing <- intersect(names(rename_map), names(merged))
  setnames(merged, old = existing, new = rename_map[existing])
  
  data_cls <- epistemic::add_covariates(
    covariates = merged,
    dc = data_cls,
    region=data_cls$region_column,
    date=data_cls$date_column
  )

  return (data_cls$data)
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

pre_process_data <- function(data, nforecasts ) {
  data <- add_fips(data)
  data[, date := as.data.table(date)]
  data$date <- as.Date(data$date)
  data_cls <- epistemic::data_class(
    data = data,
    region_column = "countyfips",
    date_column = "date",
    numerator_column = "target",
    denominator_column = "overall",
    generate_expected = TRUE
  )
  data_cls <- epistemic::add_mmwr_week(data_cls)
  

data_cls <- epistemic::add_missing_and_future_dates(
  num_future_steps = nforecasts,
  dc = data_cls,
  forward_fill = TRUE,
  den = 1
)

  return(data_cls)
}

get_formula <- function(formula_type, input) {
  
  # if this is custom, return the custom input
  if(formula_type == "custom_formula") {
    return(as.formula(input[["custom_formula"]]))
  }
  
  # Required
  region_random_effect=build_region_random_effect(
    input, 
    # use default if formula type is default, or if advance customization toggle is off
    use_default = formula_type == "default" || input[["customize_rre"]] == FALSE
  )
  
  temporal_component=build_temporal_component(
    input,
    # use default if formula type is default, or if advance customization toggle is off
    use_default = formula_type == "default" || input[["customize_temporal_component"]] == FALSE  )
  
  spatial_component=build_spatial_component(
    input,
    # use default if formula type is default, or if advance customization toggle is off
    use_default = formula_type == "default" || input[["customize_spatial_component"]] == FALSE
  )
  
  components = list(
    "intercept" = parse_expr("1"),
    "region_re" = region_random_effect,
    "spatial" = spatial_component,
    "temporal" = temporal_component
  )

  # reduce the components to those that are requested:
  requested = c("intercept")
  if(input$rre_component_chkbx == TRUE || formula_type == "default") requested = c(requested, "region_re")
  if(input$spatial_component_chkbx == TRUE || formula_type == "default") requested = c(requested, "spatial")
  if(input$temporal_component_chkbx == TRUE) requested = c(requested, "temporal")
  
  formula <- purrr::reduce(components[requested], ~call2('+', .x, .y))
  
  expr(target~!!formula)
  
}

build_region_random_effect <- function(input, use_default=FALSE) {
  
  if(use_default == TRUE) {
    for(n in names(MODEL_COMPONENT_DEFAULTS)) {
      input[[n]] = MODEL_COMPONENT_DEFAULTS[[n]]
    }
  }

  check_names(input, c("rre_prec_pc_param", "rre_prec_pc_alpha"))
  
  rlang::parse_expr(
    paste(
      "f(",
      "r_id,",
      "model='iid',", 
      "hyper=list(prec = list(prior = 'pc.prec', param =c(",
      input[["rre_prec_pc_param"]], ",", input[["rre_prec_pc_alpha"]], ")))",
      ")"
    )
  )
}

build_temporal_component <- function(input, use_default = FALSE) {
  
  if(use_default == TRUE) {
    for(n in names(MODEL_COMPONENT_DEFAULTS)) {
      input[[n]] = MODEL_COMPONENT_DEFAULTS[[n]]
    }
  }
  
  check_names(input, c("tco_model", "tco_model_ar_order"))
  
  tc = paste0(
    "f(",
    "week_id, ",
    "model = '", input[["tco_model"]], "' "
  )
  if(input[["tco_model"]] == "ar") {
    tc = paste0(tc, ", order=", input[["tco_model_ar_order"]], ")")
  } else {
    tc <-paste0(
      tc,
      ",cyclic=TRUE",
      ")"
    )
  }
  
  rlang::parse_expr(tc)
  
}

build_spatial_component <- function(input, use_default = FALSE) {
  
  if(use_default == TRUE) {
    for(n in names(MODEL_COMPONENT_DEFAULTS)) {
      input[[n]] = MODEL_COMPONENT_DEFAULTS[[n]]
    }
  }
  
  check_names(input, c("sco_model_type", "sco_control_group_model", "sco_control_group_ar_order"))

    sc = paste0(
    "f(",
    "r_id, ", 
    "graph = adjacency_matrix,",
    "model='", input[["sco_model_type"]], "',",
    "group = d_id,",
    "control.group = list(model='", input[["sco_control_group_model"]], "'"
  )
  if(input[["sco_control_group_model"]] == "ar") {
    sc = paste0(
      sc,
      ", order=", input[["sco_control_group_ar_order"]], "), "
    )
  } else sc = paste0(sc, "), ")
    
  prec_prior_name = fcase(
    input[["sco_model_type"]] == "besagproper", "prec",
    input[["sco_model_type"]] == "bym", "prec.spatial",
    input[["sco_model_type"]] == "besag", "prec"
  )
  sc = paste0(
    sc,
    "hyper=list(", prec_prior_name, " = list(prior = 'pc.prec', param =c(",
    input[["rre_prec_pc_param"]], ", ", input[["rre_prec_pc_alpha"]], ")))",
    ")"
  )
  
  rlang::parse_expr(sc)
  
}

MODEL_COMPONENT_DEFAULTS = list(
  sco_model_type = "besagproper", 
  sco_control_group_model = "ar",
  tco_model = "rw2", 
  rre_prec_pc_param = 0.2,
  rre_prec_pc_alpha = 0.01,
  sco_control_group_ar_order = 1L,
  sco_prec_pc_param = 0.2,
  sco_prec_pc_alpha = 0.01,
  tco_model_ar_order = 1L,
  custom_formula = ""
)

check_names <- function(x, n) {
  if(!all(n %in% names(x))) {
    cli::cli_abort(paste(
      "Names [", paste(n, collapse=","), " ] not found in object"
    ))
  }
}
