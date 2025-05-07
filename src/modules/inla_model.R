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
      "Binomial" = "binomial"#,
      #"Negative Binomial"="nbinomial",
      #"Poisson" = "poisson"
    ),
    selected = "binomial"
  )
  
  hidden(region_re_options <- tagList(
    div(
      class = "well",
      HTML("Precision Hyper-parameters:"),
      numericInput(ns("rre_prec_pc_param"), "PC Prior Param", value=0.2),
      numericInput(ns("rre_prec_pc_alpha"), "PC Prior Alpha", value=0.01)
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
      ns("sco_model_type"), "Model Type",
      choices = c("Proper Besag Model" = "besagproper")
    ),
    selectInput(
      ns("sco_control_group_model"), "Date-based model",
      choices = c("Autoregressive" = "ar")
    ),
    conditionalPanel(
      condition = "input.sco_control_group_model == 'ar'",
      numericInput(ns("sco_control_group_ar_order"), "Order", value = 1, min=1, max=5), 
      ns=ns
    ),
    hidden(div(
      class = "well",
      HTML("Precision Hyper-parameters:"),
      numericInput(ns("sco_prec_pc_param"), "PC Prior Param", value=0.2),
      numericInput(ns("sco_prec_pc_alpha"), "PC Prior Alpha", value=0.01)
    ))
  )
  
  temporal_component_options <- tagList(
    selectInput(
      ns("tco_model"), "Model",
      choices = c("Random Walk" = "rw2", "Autoregressive-1" = "ar1", "Autoregressive-p" = "ar"),
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
    layout_columns(
      checkboxInput(ns("include_rre"), "Region Random Effect",value = TRUE),
      # unhide this if we want to allow for customization of RRE
      hidden(input_switch(ns("customize_rre"),label ="Customize",value = FALSE)),
      col_widths = c(6,6)
    ),
    conditionalPanel(
      condition = "input.customize_rre",
      region_re_options,
      ns=ns
    ),
    layout_columns(
      checkboxInput(ns("spatial_component_chkbx"), "Spatial Component",value = FALSE),
      input_switch(ns("customize_spatial_component"),label ="Customize",value = FALSE),
      col_widths = c(6,6)
    ),
    conditionalPanel(
      condition = "input.customize_spatial_component",
      spatial_component_options, 
      ns=ns
    ),
    layout_columns(
      checkboxInput(ns("temporal_component_chkbx"), "Temporal Component",value = FALSE),
      input_switch(ns("customize_temporal_component"),label ="Customize",value = FALSE),
      col_widths = c(6,6)
    ), 
    conditionalPanel(
      condition = "input.customize_temporal_component",
      temporal_component_options,
      ns=ns
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
        #hyper_params,
        formula_panel,
        input_task_button(ns("estimate_model_btn"), "Run Model")
      ),
      layout_column_wrap(
        width=NULL, height=300, 
        style = css(grid_template_columns = c("60%", "40%")),
        model_data_card,
        layout_column_wrap(
          width=1, 
          #heights_equal = "row",
          model_card, model_formula_card
        ) 
      )
    )
  )  
}

inla_model_server <- function(id, dc, im, results) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # the RRE is required, so disable the checkbox for now.
      disable(id = "include_rre")
      
      observe(im$model <- inla_model()$model)
      observe(im$posterior <- get_posteriors(
        res_data = inla_model()[["processed_data"]],
        inla_model = inla_model()[["model"]],
        date_col = inla_model()[["date_col"]],
        family = input$dist_family,
        suffix=NULL
        )) |> bindEvent(inla_model())
      
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
        processed_data <- pre_process_data(results$data, input$nforecasts)
        
        #3 Get adjacency matrix (only needed if spatial is requested)
        if(input$formula_type == "default") {
          adj_mat_inla <- get_adjacency_dt(processed_data$data, processed_data$region_col)
        } else if (input$spatial_component_chkbx == TRUE) {
          if(input$sco_adjacency_type == "mobility_adj_mat") {
            adj_mat_inla <- get_adjacency_dt(processed_data$data, processed_data$region_col)
          } else {
            adj_mat_inla <- get_physical_adjacency_dt(processed_data$data, processed_data$region_col)
          }
        } else {
          "not getting adjacency matrix"
        }
        
        formula <- get_formula(
          formula_type = input$formula_type,
          input = setNames(
            lapply(names(input), \(n) input[[n]]),
            names(input)
          )
        )
        
        formula = eval(formula)
        
        #5. fit the model
        print("fitting model")
        
        model <- fit_model(data=processed_data$data,formula=formula,family=input$dist_family)
        
        return(list(
          model = model,
          processed_data = processed_data[["data"]],
          region_col = processed_data[["region_col"]],
          date_col = processed_data[["date_col"]], 
          formula = deparse1(formula)
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
          inla_model()$processed_data,
          rownames=FALSE
        )
      })
      
      output$download_data <- downloadHandler(
        filename = "processed_data.csv" ,
        content = \(file) data.table::fwrite(inla_model()$processed_data, file)
      )
      
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

pre_process_data <- function(data, nforecasts ) {
  
  data <- expand_dataset(data,nforecasts)
  date_col="date"
  region_col="region"
  cat_cols=c(date_col,region_col)
  
  for (col in cat_cols){ # add id columns
    data[[paste0(col,"_id")]]=as.numeric(factor(data[[col]])) 
    data[[paste0(col,"_id2")]]=as.numeric(factor(data[[col]]))
  }
  data<-add_fips(data) # add column with fips codes
  data<-add_expected(data,nforecasts) # add column with expected counts (based on share of total counts) for each region
  data$week_id <- (data$date_id - 1) %% 52 + 1
  
  return(list(data = data, region_col = region_col, date_col = date_col, cat_cols = cat_cols))
}

get_formula <- function(formula_type, input) {
  
  print("creating_formula")
  
  # if this is custom, return the custom input
  if(formula_type == "custom_formula") {
    return(as.formula(input[["custom_formula"]]))
  }
  
  # Required
  region_random_effect=build_region_random_effect(input, use_default = formula_type == "default")
  temporal_component=build_temporal_component(input, use_default = formula_type == "default")
  spatial_component=build_spatial_component(input,  use_default = formula_type == "default")
  
  components = list(
    "intercept" = parse_expr("1"),
    "region_re" = region_random_effect,
    "spatial" = spatial_component,
    "temporal" = temporal_component
  )

  # reduce the components to those that are requested:
  requested = c("intercept", "region_re")
  if(input$spatial_component_chkbx == TRUE || formula_type == "default") requested = c(requested, "spatial")
  if(input$temporal_component_chkbx == TRUE || formula_type == "default") requested = c(requested, "temporal")
  
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
      "region_id2,",
      "model='iid',", 
      "hyper=list(prec = list(prior = 'pc.prec', param =", input[["rre_prec_pc_param"]], ", alpha = ", input[["rre_prec_pc_alpha"]], "))",
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
    tc = paste0(tc, ", order=", input[["tco_model_ar_order"]])
  }
  if(input[["tco_model"]] == "rw2" || input[["tco_model"]] == "ar1") {
    tc <-paste0(
      tc,
      ",cyclic=TRUE",
      ")"
    )
  } else {
      tc <- paste0(tc,")")
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
    "region_id, ", 
    "graph = adj_mat_inla,",
    "model='", input[["sco_model_type"]], "',",
    "group = date_id,",
    "control.group = list(model='", input[["sco_control_group_model"]], "'"
  )
  if(input[["sco_control_group_model"]] == "ar") {
    sc = paste0(
      sc,
      ", order=", input[["sco_control_group_ar_order"]], "), "
    )
  } else sc = paste0(sc, ")")
  sc = paste0(
    sc,
    "hyper=list(prec = list(prior = 'pc.prec', param =", input[["sco_prec_pc_param"]], ", alpha = ", input[["sco_prec_pc_alpha"]], "))", 
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
