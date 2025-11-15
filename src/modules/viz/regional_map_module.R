# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958
viz_regional_map_ui <- function(id) {
  ns <- NS(id)
  
  spark_card <- card(
    style = "width: 100%; height:100px;",
    class = "mb-1",
    card_body(
      style = "height:100%; padding: 0;",
      plotlyOutput(ns("date_spark"), height = "100%", width = "100%")
    )
  )
  
  metric_buttons <- radioButtons(
    inputId = ns("map_metric"),
    label="Metric",
    choices = c(
      "Mean" = "mean", 
      "Median" = "median",
      "Quantile" = "quantile"
      #"Exceedance" = "exceedance"
    ),
    inline = FALSE,
    selected = "mean"
  )
  
  metric_count_type <- radioButtons(
    inputId = ns("metric_counts"),
    label="Counts/Proportions",
    choices = c("Counts", "Proportion"),
    selected = "Counts",
    inline = TRUE
  )
  
  
  nav_panel(
    title = "Region-Wide Map",
    
    layout_sidebar(
      leafletOutput(ns("region_map")),
      sidebar = sidebar(
        id=ns("region_map_sidebar"),
        width = SIDEBAR_WIDTH*2,
        div(
          "Date Selection",
          style = "font-weight: 600; font-size: 0.95rem; margin-bottom: 6px;"
        ),
        spark_card, 
        sliderInput(
          inputId = ns("map_date_slider"),
          label = NULL,
          min   = as.Date("1970-01-01"),
          max   = as.Date("1970-01-02"),
          value = as.Date("1970-01-02"),
          step  = 1,
          timeFormat = "%Y-%m-%d",
          ticks = FALSE
        ),
        tags$style(HTML(sprintf(
          "#%s .irs-min, #%s .irs-max { display: none !important; }",
          ns("map_date_slider"), ns("map_date_slider")))),
        
        metric_buttons,
        metric_count_type,
        conditionalPanel(
          condition = "input.map_metric == 'quantile'",
          sliderInput(
            inputId = ns("metric_quantile"),label = "Quantile",min = 0,max=1,step = 0.01,value=0.5
          ),
          ns = ns
        ),
        # conditionalPanel(
        #   condition = "input.map_metric == 'exceedance'",
        #   uiOutput(outputId = ns("metric_exceedance_thresh_ui")),
        #   ns = ns
        # ),
        position = "left"
      )
    )
  )
}

viz_regional_map_server <- function(id, im, results) {
  moduleServer(
    id,
    function(input, output, session) {
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
      }) |> bindEvent(input$metric_counts,ignoreNULL = F)
      
      value_colname <- reactive({
        if (identical(input$metric_counts, "Counts")) {
          if (!is.null(im$data_cls$count_col)) im$data_cls$count_col else "count"
        } else {
          if (!is.null(im$data_cls$prop_col)) im$data_cls$prop_col else "proportion"
        }
      })
      
      all_dates <- reactive({
        req(im$data_cls)
        as.Date(sort(unique(im$data_cls$data[[im$data_cls$date_col]])))
      })
      
      observe({
        d <- all_dates()
        validate(need(length(d) > 0, "No dates found"))
        updateSliderInput(
          session,
          inputId = "map_date_slider",
          min   = min(d, na.rm = TRUE),
          max   = max(d, na.rm = TRUE),
          value = max(d, na.rm = TRUE),
          step  = 1
        )
      })
      
      target_date <- reactive({
        req(input$map_date_slider)
        d <- all_dates()
        t <- as.Date(input$map_date_slider)
        
        # if selection is not in the model dates, snap to nearest
        if (!(t %in% d) && length(d)) {
          d[which.min(abs(d - t))]
        } else {
          t
        }
      })
      
      
      output$date_spark <- renderPlotly({
        req(im$data_cls)
        date_col <- im$data_cls$date_col
        num_col <- im$data_cls$numerator_col
        den_col <- im$data_cls$denominator_col
        
        default_color <- "#636EFA"
        
        dt <- data.table::as.data.table(im$data_cls$data)
        if (!(date_col %in% names(dt))) return(NULL)
        
        # get the spark series, depending on counts/proportion        
        
        if(input$metric_counts == "Counts") {
          series <- dt[, .(total = sum(x, na.rm = TRUE)), by = c(date_col), env=list(x=num_col)]
          yaxis_title = "Total Cases"
        } else {
          series <- dt[, .(total = sum(x, na.rm = TRUE)/sum(y, na.rm=TRUE)), by = c(date_col), env=list(x=num_col, y=den_col)]
          yaxis_title = "Percent of ED Visits"
        }
        data.table::setnames(series, date_col, "date")
        data.table::setorder(series, date)
        
        p <- plotly::plot_ly(
          series,
          x = ~date, y = ~total,
          type = "scatter", mode = "lines",
          line = list(color = default_color),
          hoverinfo = "none"
        ) |>
          plotly::layout(
            margin = list(l = 28, r = 6, t = 4, b = 22),
            #margin = list(l = 0, r = 0, t = 0, b = 0),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)",
            xaxis = list(
              title = list(text = "", font = list(color=default_color, size=10)),
              showticklabels = FALSE, ticks = "", showgrid = FALSE, zeroline = FALSE
            ),
            yaxis = list(
              title = list(text = yaxis_title, font=list(color=default_color, size=10)),
              showticklabels = FALSE, ticks = "", showgrid = FALSE, zeroline = FALSE
            ),
            showlegend = FALSE
          ) |> 
          plotly::config(displayModeBar = FALSE)
        return(p)
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
      
      map_data <- reactive({
        
        req(im$model)
        
        params <- list(
          metric = input$map_metric,
          use_count = input$metric_counts == "Counts",
          quantile = input$metric_quantile,
          threshold = 10 # place holder as constant exceedance threshold
        )
        
        md <- get_map_data(
          model = im$model,
          data_cls = im$data_cls,
          params = params
        )
        return(md)
      })
      
      map_base_locations <- reactive({
        
        req(im$data_cls)
        get_map_locations(im$data_cls)
        
      })
      
      
      region_map <- reactive({
        
        req(map_base_locations(), map_data(), target_date())
        
        pi = polygon_info(map_base_locations(),map_data(),target_date())
        
        leaflet::leaflet() |>
          leaflet::addProviderTiles("CartoDB.Positron") |>
          update_polygons(pi) |>
          leaflet.extras::addFullscreenControl() |>
          leaflet.extras::addResetMapButton()
      })
      
      output$region_map <- renderLeaflet({
        validate(need(im$posterior, "Load data and run model first"))
        region_map()
      })
      
      observe({
        req(region_map())
        leafletProxy("region_map") |>
          clearControls() |>
          clearShapes() |>
          update_polygons(
            polygon_info(map_base_locations(), map_data(), target_date())
          )
      })
      
      
    }
  )
}
