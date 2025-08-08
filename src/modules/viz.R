viz_ui <- function(id) {
  btn_class <- "btn-primary btn-sm"
  ns <- NS(id)
  
  nav_panel(
    title = "Visualization",
    navset_bar(
      nav_panel(
        title = "Region-Wide Map",
        layout_sidebar(
          leafletOutput(ns("region_map")),
          sidebar = sidebar(
            id=ns("region_map_sidebar"),
            width = SIDEBAR_WIDTH*2,
            selectInput(ns("map_date"),label="Map Date", choices = NULL),
            radioButtons(
              inputId = ns("map_metric"),
              label="Metric",
              choices = c(
                "Mean" = "mean", 
                "Median" = "median",
                "Quantile" = "quantile",
                "Exceedance" = "exceedance"
              ),
              inline = FALSE,
              selected = "mean"
            ),
            radioButtons(
              inputId = ns("metric_counts"),
              label="Counts/Proportions",
              choices = c("Counts", "Proportion"),
              selected = "Counts",
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.map_metric == 'quantile'",
              sliderInput(
                inputId = ns("metric_quantile"),label = "Quantile",min = 0,max=1,step = 0.01,value=0.5
              ),
              ns = ns
            ),
            #uiOutput(outputId = ns("metric_exceedance_thresh_ui")),
            conditionalPanel(
              condition = "input.map_metric == 'exceedance'",
              uiOutput(outputId = ns("metric_exceedance_thresh_ui")),
              ns = ns
            ),
            position = "left"
          )
        )
      ),
      nav_panel(
        title = "Time Series Plots",
        layout_sidebar(
          sidebar = sidebar(
            id = ns("time_series_sidebar"),
            width = SIDEBAR_WIDTH*2,
            # checkboxInput(ns("add_temporal"), "Add Temporal Trend",value = FALSE),
            # checkboxInput(ns("add_rolling"), "Add Rolling Average",value = FALSE),
            # checkboxInput(ns("add_rescaled"), "Add Rescale",value = FALSE),
            radioButtons(ns("ts_use_count"),"Scale", choices = c("Count", "Proportion"),selected = "Proportion"),
            #numericInput(ns("ts_future_steps"), "Future Steps", value=0, min=0),
            selectInput(
              ns("ts_quantile"), "CI", 
              choices = c("99%" = "99", "95%" = "95", "90%" = "90", "50%" = "50"),
              selected = "95"
            ),
            selectizeInput(ns("viz_regions"), "Select Region(s)", choices=NULL, multiple=TRUE)
          ),
          card(
            plotlyOutput(ns("ts_plots")),
            # fix time series toggle not working yet, so hidden
            card_footer(hidden(input_switch(ns("fix_ts_y_axis"),"Fix y-axis",value=FALSE)))
          )
        ),
      ),
      nav_panel(
        title="Posterior Data",
        card(
          card_body(DTOutput(ns("posterior_data"))),
          card_footer(downloadButton(ns("download_posterior_btn"), "Download", class = "btn-primary"))
        )
      ),
      navbar_options = list(class = "bg-primary", theme = "dark", underline=FALSE)
    )
  )
  
}

viz_server <- function(id, dc, im, results) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns = session$ns
      
      observe({
        all_dates= im$data_cls$data[[im$data_cls$date_col]] |> unique()
        updateSelectInput(
          inputId = "map_date",
          choices = all_dates,
          selected = max(all_dates)
        )
      })
      
      # Render the exceedance threshold ui widget
      # Note that when metrics is count, then this should be numericInput
      # but when metrics is Proportion, then this should a slider from 0 to 1
      output$metric_exceedance_thresh_ui <- renderUI({
        
        if(input$metric_counts == "Counts") {
          widget <- numericInput(ns("metric_exceedance"), label = "Exceedance Threshold", value=10)
        } else {
          widget <- sliderInput(ns("metric_exceedance"), label = "Exceedance Threshold", min = 0,
                      max=1, value=0.5,step = .001)
        }
        return(widget)
      })
      
      
      input_region_choices <- reactive(
        im$data_cls$data[, .(countyfips, region)] |> unique()
      )
      
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
      
      output$posterior_data <- renderDT({
        req(im$posterior)
        im$posterior
      })
      

      map_data <- reactive({
        req(im$model)
        
        params =  list(
          metric = input$map_metric,
          use_count = input$metric_counts == "Counts",
          quantile = input$metric_quantile,
          threshold = input$metric_exceedance
        )
        print(params)
        
        get_map_data(
          model = im$model,
          data_cls = im$data_cls,
          params = params
        )
      })
      
      map_base_locations <- reactive({
        
        req(im$data_cls)
        get_map_locations(im$data_cls)
      
      }) |> bindEvent(im$data_cls)
      

      target_date <- reactive(input$map_date)
      
      region_map <- reactive({
        pi = polygon_info(map_base_locations(),map_data(),target_date())
        
        leaflet::leaflet() |>
          leaflet::addProviderTiles("CartoDB.Positron") |> 
          update_polygons(pi) |> 
          leaflet.extras::addFullscreenControl() |> 
          leaflet.extras::addResetMapButton()
      })
      
      output$region_map <- renderLeaflet({
        region_map()
        
      }) |> bindEvent(map_base_locations())
      
      observe({
        req(map_base_locations())
        req(map_data())
        req(target_date())
        leafletProxy("region_map") |>
          clearControls() |>
          clearShapes() |>
          update_polygons(
            polygon_info(map_base_locations(), map_data(), target_date())
          )
      }) |> bindEvent(map_data(), target_date(), ignoreInit = FALSE)
      

      tspd <- reactive({
        req(im$posterior)
        prepare_plot_ly_ts_data(
          im$model, im$data_cls, 
          use_count = input$ts_use_count == "Count", 
          future_steps=im$nforecasts,
          display_col = "region"
        )
      }) |> bindEvent(input$ts_use_count)
      
      
      output$ts_plots <- renderPlotly({
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
      
      
      # Download posterior data
      output$download_posterior_btn <- downloadHandler(
        filename = "posterior_data.csv" , content = \(file) data.table::fwrite(im$posterior, file)
      )
      

    }
  )
}
