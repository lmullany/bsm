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
  
  #hyper_params = tagList(
  #  numericInput(ns("param"), "INLA Hyperparam: param", value=0.2),
  #  numericInput(ns("alpha"), "INLA Hyperparam: alpha", value=0.01),
  #)
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
        #hyper_params,
        formula_panel,
        input_task_button(ns("estimate_model_btn"), "Run Model")
      ),
      card(
        card_header("INLA Model Object", class="bg-primary"),
        card_body(withSpinner(
          verbatimTextOutput(ns("inla_model_object")),
          caption = "Estimating Model ... please wait",
          color = bs_get_variables(theme=THEME,"primary")
        ))
      )
    )
  )  
}

inla_model_server <- function(id, dc, im, results) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
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
        
        #1. TODO: VALIDATE INPUTS
        #2. Preprocess Data:
        print("getting_pre-processed data")
        processed_data <- pre_process_data(results$data, input$nforecasts)
        
        print(processed_data)
        
        
        #3 Get adjaceny matrix
        print("getting_adjacency_matrix")
        adj_mat_inla<-get_adjacency_dt(processed_data$data, processed_data$region_col)
        
        
        #4. Create formula (for now, just using default)
        #formula = create_formula(inputs...)
        print("creating_formula")
        alpha = 0.01
        param = 0.2
        formula = target ~ 1 + 
          f(
            region_id,
            graph = adj_mat_inla,
            model='besagproper',
            group = date_id,
            control.group = list(model='ar1'),
            #hyper=list(prec = list(prior = 'pc.prec', param =input$param, alpha = input$alpha))
            hyper=list(prec = list(prior = 'pc.prec', param =param, alpha = alpha))
          ) +
          f(
            region_id2,
            model='iid', 
            #hyper=list(prec = list(prior = 'pc.prec', param =input$param, alpha = input$alpha))
            hyper=list(prec = list(prior = 'pc.prec', param =param, alpha = alpha))
          ) +
          f(
            week_id,
            model='rw2',
            cyclic=TRUE
          )
        
        
        #5. fit the model
        print("fitting model")
        
        model <- fit_model(data=processed_data$data,formula=formula,family=input$dist_family)
        
        return(list(
          model = model,
          processed_data = processed_data[["data"]],
          region_col = processed_data[["region_col"]],
          date_col = processed_data[["date_col"]]
        ))
        

      }) |> bindEvent(input$estimate_model_btn)
    
    
      output$inla_model_object <- renderPrint({
        req(inla_model())
        print(summary(inla_model()$model))
      })
      
      output$raw_data <- renderDT(results$data)
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
  
  print(paste0("the class of data is ", class(data)))
  
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