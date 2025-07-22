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
            conditionalPanel(
              condition = "input.map_metric != 'exceedance'",
              radioButtons(
                inputId = ns("metric_counts"),
                label="Counts/Proportions",
                choices = c("Counts", "Proportion"),
                selected = "Counts"
              ),
              ns = ns
            ),
            conditionalPanel(
              condition = "input.map_metric == 'quantile'",
              sliderInput(
                inputId = ns("metric_quantile"),label = "Quantile",min = 0,max=1,step = 0.01,value=0.5
              ),
              ns = ns
            ),
            conditionalPanel(
              condition = "input.map_metric == 'exceedance'",
              numericInput(inputId = ns("metric_exceedance"), label = "Exceedance Threshold", value=10),
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
            numericInput(ns("ts_future_steps"), "Future Steps", value=0, min=0),
            numericInput(ns("ts_quantile"), "Quantile", value=0.95),
            selectizeInput(ns("viz_regions"), "Select Region(s)", choices=NULL, multiple=TRUE)
          ),
          #plotOutput(ns("plots"))
          plotlyOutput(ns("ts_plots"))
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
      
      observe({
        updateNumericInput(
          inputId = "ts_future_steps",
          value = im$nforecasts
        )
      })
      
      observe({
        all_dates= im$data_cls$data[[im$data_cls$date_col]] |> unique()
        updateSelectInput(
          inputId = "map_date",
          choices = all_dates,
          selected = max(all_dates)
        )
      })
      
      observe({
        choices = md$data_class$data[, .(countyfips, region)] |> unique()
        choices = setNames(choices$countyfips, choices$region)
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
        get_map_data(
          model = im$model,
          data_cls = im$data_cls,
          params =  list(
            metric = input$map_metric,
            use_count = input$metric_counts == "Counts",
            quantile = input$metric_quantile,
            threshold = input$metric_exceedance
          )
        )
      })
      
      map_base_locations <- reactive({
        
        req(im$data_cls)
        
        locs = im$data_cls$data[, .(countyfips, region)] |> unique()
        
        map_data <- dplyr::right_join(
          mutate(county_sf, countyfips = paste0(STATEFP, COUNTYFP)),
          locs,
          by="countyfips"
        ) |> 
          sf::st_transform(crs = 4326)
      }) |> bindEvent(im$data_cls)
      

      target_date <- reactive(input$map_date)
      
      output$region_map <- renderLeaflet({
        
        pi = polygon_info(map_base_locations(),map_data(),target_date())
        
        leaflet::leaflet() |>
          leaflet::addProviderTiles("CartoDB.Positron") |> 
          update_polygons(pi) |> 
          leaflet.extras::addFullscreenControl() |> 
          leaflet.extras::addResetMapButton()
        
      }) |> bindEvent(map_base_locations())
      
      observe({
        leafletProxy("region_map") |>
          clearControls() |>
          clearShapes() |>
          update_polygons(
            polygon_info(map_base_locations(), map_data(), target_date())
          )
      }) |> bindEvent(map_data(), target_date(), ignoreInit = TRUE)
      

      # For now, making all the plots, because they are fast
      plots <- reactive({
        req(im$posterior)
        make_timeseries_plots(res_data = im$posterior, date_col = "date", use_prop = TRUE, F, F, F)
      })
      
      # output$plots <- renderPlot({
      #   req(input$viz_regions)
      #   req(plots())
      #   subplots <- plots()[input$viz_regions]
      #   do.call("grid.arrange", c(subplots, ncol = min(c(2, length(subplots)))))
      # })
      
      tspd <- reactive({
        req(im$posterior)
        prepare_plot_ly_ts_data(
          im$model, im$data_cls, 
          use_count = input$ts_use_count == "Count", 
          future_steps=as.integer(input$ts_future_steps), 
          quantile = input$ts_quantile
        )
      }) |> bindEvent(input$ts_use_count, input$ts_quantile, input$ts_future_steps)
      
      
      output$ts_plots <- renderPlotly({
        print(tspd())
        time_series_subplots(input$viz_regions, ts_plot_data = tspd())
      })
      
      
      # Download posterior data
      output$download_posterior_btn <- downloadHandler(
        filename = "posterior_data.csv" , content = \(file) data.table::fwrite(im$posterior, file)
      )
      

    }
  )
}
