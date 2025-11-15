# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958
viz_time_series_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    title = "Time Series Plots",
    layout_sidebar(
      sidebar = sidebar(
        id = ns("time_series_sidebar"),
        width = SIDEBAR_WIDTH*2,
        radioButtons(ns("ts_use_count"),"Scale", choices = c("Count", "Proportion"),selected = "Proportion"),
        #numericInput(ns("ts_future_steps"), "Future Steps", value=0, min=0),
        numericInput(ns("ts_quantile"), "Quantile", value=0.95),
        selectizeInput(ns("viz_regions"), "Select Region(s)", choices=NULL, multiple=TRUE)
      ),
      card(
        plotlyOutput(ns("ts_plots")),
        # fix time series toggle not working yet, so hidden
        card_footer(hidden(input_switch(ns("fix_ts_y_axis"),"Fix y-axis",value=FALSE)))
      )
    ),
  )
  
}

viz_times_series_server <- function(id, im, results) {
  moduleServer(
    id,
    function(input, output, session) {
      # update the label for credible interval when count/proportion changes
      observe({
        updateSelectInput(
          inputId = "ts_quantile",
          label=paste0("Credible Interval for Mean ", input$ts_use_count)
        )
      }) |> bindEvent(input$ts_use_count)
      
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
        time_series_subplots(input$viz_regions, ts_plot_data = tspd(), ci = input$ts_quantile,display_col = "region")
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
