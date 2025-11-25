# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

label_list_ts <- list(
  scale_radio = list(
    l = "Scale",
    m = "Select the desired scale for the displayed results. Select count to display the number of visits and proportion to display the proportion of visits with the requested diagnosis out of all visits. Note that forecasts are only available when the scale matches the natural scale of the model, i.e. count for poisson and negative binomial and proportion for binomial and beta binomial."
  ),
  ci_Count =  list(
    l = "Credible Interval for Mean Count",
    m = "Select the width of the credible interval for the mean count of the posterior distribution to display on the plot."
  ),
  ci_Proportion =  list(
    l = "Credible Interval for Mean Proportion",
    m = "Select the width of the credible interval for the mean proportion of the posterior distribution to display on the plot."
  ),
  region_selector =  list(
    l = "Select Region(s)",
    m = "Click or type the name(s) of the regions you would like to plot using the dropdown."
  )
)
viz_time_series_ui <- function(id) {
  ns <- NS(id)
  
  # helper function
  custom_quantiles <- function(probs = c(0.005, 0.01, 0.025,seq(0.05, .95, 0.05),0.975, 0.99, 0.995)) {
    vals = 100*rev(sapply(seq(1, length(probs)/2,1), \(i) rev(probs)[i] - probs[i]))
    names = sprintf("%2.0f%%",vals)
    setNames(vals, names)
  }
  
  nav_panel(
    title = "Time Series Plots",
    layout_sidebar(
      sidebar = sidebar(
        id = ns("time_series_sidebar"),
        width = SIDEBAR_WIDTH*2,
        radioButtons(ns("ts_use_count"),
                     labeltt(label_list_rm[["scale_radio"]]),
                     choices = c("Count", "Proportion"),selected = "Proportion",
                     inline=TRUE),
        radioButtons(
          inputId = ns("ts_quantile"),
          label = "Quantile",
          choices = c("99%" = 99, "95%" = 95, "90%" = 90, "50%" = 50, "Other"),
          selected = 95,
          inline = TRUE
        ),
        conditionalPanel(
          condition = "input.ts_quantile=='Other'",
          selectInput(
            inputId = ns("custom_ts_quantile"), 
            label="Custom CI Width",
            choices = custom_quantiles(),
            selected = 95
          ),
          ns = ns
        ),
        #numericInput(ns("ts_quantile"), "Quantile", value=0.95),
        selectizeInput(ns("viz_regions"), 
                       labeltt(label_list_ts[["region_selector"]]),
                       choices=NULL, 
                       multiple=TRUE)
      ),
      card(
        plotlyOutput(ns("ts_plots")),
        # fix time series toggle not working yet, so hidden
        card_footer(hidden(input_switch(ns("fix_ts_y_axis"),"Fix y-axis",value=FALSE)))
      )
    ),
  )
  
}

viz_time_series_server <- function(id, im, results) {
  moduleServer(
    id,
    function(input, output, session) {
      # update the label for credible interval when count/proportion changes
      observe({
        updateRadioButtons(
          inputId = "ts_quantile",
          label=labeltt(label_list_ts[[paste0("ci_",input$ts_use_count)]]),
        )
      }) |> bindEvent(input$ts_use_count)
      
      # get the numeric version of quantile (either preset or custom)
      ts_quantile<- reactive({
        req(input$ts_quantile)
        fifelse(
          input$ts_quantile == "Other",
          as.numeric(input$custom_ts_quantile),
          as.numeric(input$ts_quantile)
        )
      })
      
      # get the region choices
      input_region_choices <- reactive(
        im$data_cls$data[, .(countyfips, region)] |> unique()
      )
      
      # Update the region choices based on the data
      # TODO: note that this assumes county
      observe({
        req(im$data_cls)
        choices = setNames(input_region_choices()$countyfips, input_region_choices()$region)
        updateSelectizeInput(
          inputId = "viz_regions",
          choices = choices,
          selected = choices[1]
        )
      })
      
      # Get the plotly data
      tspd <- reactive({
        req(im$posterior)
        prepare_plot_ly_ts_data(
          im$model, im$data_cls, 
          use_count = input$ts_use_count == "Count", 
          future_steps=im$nforecasts
        )
      }) |> bindEvent(input$ts_use_count)
      
      
      output$ts_plots <- renderPlotly({
        validate(need(im$posterior, "Load data and run model first"))
        time_series_subplots(
          input$viz_regions,
          ts_plot_data = tspd(),
          display_col = "region",
          ci = ts_quantile()

        )
      })
      
      # proxy to change the yaxes from varied to fixed does not
      # currently work well
      # observe({
      #   
      #   req(input$viz_regions)
      #   req(tspd())
      # 
      #   n_axes <- length(input$viz_regions)
      #   axis_names <- if (n_axes == 1) "yaxis" else paste0("yaxis", c("", 2:n_axes))
      #   
      #   if(input$fix_ts_y_axis == TRUE) {
      #     # Extract the y range from the data
      #     max_y_vals <- unlist(lapply(input$viz_regions, function(name) max(tspd()[[name]]$upper*1.1)))
      #     fixed_range <- list(range = range(0, max(max_y_vals, na.rm=TRUE)))
      #   
      #     # Construct the relayout update: yaxis, yaxis2, yaxis3, ...
      #     layout_updates <- setNames(
      #       replicate(n_axes, fixed_range, simplify = FALSE), 
      #       axis_names
      #     )
      #   } else {
      #     layout_updates <- setNames(
      #       replicate(n_axes, list(autorange = TRUE), simplify = FALSE),
      #       axis_names
      #     )
      #   }
      #   
      #   # Use plotlyProxy to update the layout
      #   plotlyProxy("ts_plots", session) |> plotlyProxyInvoke("relayout", layout_updates)
      #   
      #   
      # }) |> bindEvent(input$fix_ts_y_axis)
      
    }
  )
}
