# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958


label_list_rm <- list(
  stat_radio = list( 
    l = "Statistic",
    m = "Select the statistic to display on the choropleth map. Hover to see the specific value."
  ),
  scale_radio = list(
    l = "Scale",
    m = paste(
      "Select the desired scale for the displayed results. Select count to display 
      the number of visits and proportion to display the proportion of visits with
      the requested diagnosis out of all visits. Note that forecasts are only 
      available when the scale matches the natural scale of the model, i.e. count
      for poisson and negative binomial and proportion for binomial and beta
      binomial.
      ", sep="")
  ),
  date_sparkline =  list(
    l = "Date Selection",
    m = paste("
    The slider can be used to select the date to display on the map. The 
    sparkline indicates the overall level across all selected regions at each 
    date.",sep="")
  ),
  exceedance_thresh_n =  list(
    l = "Exceedance Threshold (N)",
    m = "Threshold Count for which the probability of exceeding should be estimated"
  ),
  exceedance_thresh_p =  list(
    l = "Exceedance Threshold (p)",
    m = "Threshold proportion for which the probability of exceeding should be estimated"
  )
  
)

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
    label=labeltt(label_list_rm[["stat_radio"]]),
    choices = c(
      "Mean" = "mean", 
      "Median" = "median",
      "Quantile" = "quantile",
      "Exceedance" = "exceedance"
    ),
    inline = FALSE,
    selected = "mean"
  )
  
  metric_count_type <- radioButtons(
    inputId = ns("metric_counts"),
    label=labeltt(label_list_rm[["scale_radio"]]),
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
          labeltt(label_list_rm[["date_sparkline"]]),
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
        
        metric_count_type,
        metric_buttons,
        conditionalPanel(
          condition = "input.map_metric == 'quantile'",
          sliderInput(
            inputId = ns("metric_quantile"),label = "Quantile",min = 0,max=1,step = 0.01,value=0.5
          ),
          ns = ns
        ),
        conditionalPanel(
          condition = "input.map_metric == 'exceedance'",
          uiOutput(outputId = ns("metric_exceedance_thresh_ui")),
          ns = ns
        ),
        tags$hr(),
        tags$details(
          tags$summary("Advanced map settings"),
          
          checkboxInput(
            inputId = ns("map_use_global_range"),
            label   = "Use shared colorbar range across all dates",
            value   = TRUE
          ),
          
          selectInput(
            ns("map_legend_position"),
            label = "Legend location",
            choices = c(
              "Bottom right" = "bottomright",
              "Bottom left"  = "bottomleft",
              "Top right"    = "topright",
              "Top left"     = "topleft"
            ),
            selected = "bottomright"
          ),
          selectInput(
            inputId = ns("map_palette"),
            label   = "Color palette",
            choices = c(
              "Viridis" = "viridis",
              "Plasma" = "plasma",
              "Magma" = "magma",
              "Inferno" = "inferno",
              "Cividis" = "cividis",
              "Greys" = "Greys",
              "YlOrRd" = "YlOrRd",
              "RdBu" = "RdBu"
            ),
            selected = "viridis"
          ),
          tags$br()
        ),
        position = "left"
      )
    )
  )
}

viz_regional_map_server <- function(id, dc, im, results) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns = session$ns
      
      # Render the exceedance threshold ui widget
      # Note that when metrics is count, then this should be numericInput
      # but when metrics is Proportion, then this should a slider from 0 to 1
      output$metric_exceedance_thresh_ui <- renderUI({
        
        # get default values for the count threshold slider, and the p
        tcol = im$data_cls[["numerator_column"]]
        dcol = im$data_cls[["denominator_column"]]
        
        # set the target for the count threshold numeric Input to the rounded median
        t_count = round(median(im$data_cls$data[[tcol]], na.rm=TRUE),0)
        # set the target for the p threshold slider to the median
        d = im$data_cls$data[!is.na(y) & y>0 & !is.na(x), env=list(x = tcol, y=dcol)]
        t_prop = round(median(d[, x/y, env=list(x=tcol, y=dcol)], na.rm=TRUE),3)
        
        
        if(input$metric_counts == "Counts") {
          widget <- numericInput(
            ns("metric_exceedance"),
            label = labeltt(label_list_rm[["exceedance_thresh_n"]]),
            value=t_count
          )
        } else {
          widget <- sliderInput(
            ns("metric_exceedance"),
            label = labeltt(label_list_rm[["exceedance_thresh_p"]]),
            min = 0,max=min(c(1,10*t_prop)),
            value=t_prop,
            step = .001
          )
        }
        return(widget)
      }) |> bindEvent(input$metric_counts,ignoreNULL = F)
      
      exceedance_threshold <- reactive(
        as.numeric(input$metric_exceedance)
      )
      
      value_colname <- reactive({
        if (identical(input$metric_counts, "Counts")) {
          if (!is.null(im$data_cls$count_col)) im$data_cls$count_col else "count"
        } else {
          if (!is.null(im$data_cls$prop_col)) im$data_cls$prop_col else "proportion"
        }
      })
      
      map_value_range <- reactive({
        md_info <- map_data()
        if (isTRUE(input$map_use_global_range)) {
          if (is.finite(md_info$max) && md_info$max > 0) {
            return(c(0, md_info$max))
          }
          return(c(0, 1))
        }
        pi <- polygon_info(
          map_base_locations(),
          map_data(),
          target_date()
        )
        
        if (!is.null(pi$minv) && !is.null(pi$maxv) &&
            is.finite(pi$maxv) && pi$maxv > 0) {
          return(c(max(0, pi$minv), pi$maxv))
        }
        if (!is.null(pi$d) && "outcome" %in% names(pi$d) &&
            length(pi$d$outcome) > 0 && any(is.finite(pi$d$outcome))) {
          rng <- range(pi$d$outcome, na.rm = TRUE)
          if (all(is.finite(rng)) && rng[2] > 0) {
            return(c(max(0, rng[1]), rng[2]))
          }
        }
        c(0, 1)
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
      
      
      map_data <- reactive({
        
        req(im$model)
        
        
        params <- list(
          metric = input$map_metric,
          use_count = input$metric_counts == "Counts",
          quantile = input$metric_quantile,
          threshold = exceedance_threshold()
        )
        if(isolate(input$map_metric) == "exceedance") {
          req(exceedance_threshold())
        }
        
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
        req(map_base_locations(), map_data(), target_date(), map_value_range())
        pi = polygon_info(map_base_locations(),map_data(),target_date())
        m <- leaflet::leaflet()
        if(dc$includes_alaska_hawaii == FALSE) {
          m <- leaflet::addProviderTiles(m, "CartoDB.Positron")
        }
        
        update_polygons(
          p       = m,
          pi      = pi,
          domain  = map_value_range(),
          palette = input$map_palette %||% "viridis"
        ) |>
          leaflet.extras::addFullscreenControl() |>
          leaflet.extras::addResetMapButton()
      })
      
      
      observe({
        req(map_base_locations(), map_data(), target_date(), map_value_range())
        
        pi <- polygon_info(
          map_base_locations(),
          map_data(),
          target_date()
        )
        
        proxy <- leafletProxy("region_map")
        proxy <- clearControls(proxy)
        proxy <- clearShapes(proxy)
        
        pal_name <- if (is.null(input$map_palette) || input$map_palette == "") {
          "viridis"
        } else {
          input$map_palette
        }
        
        proxy <- update_polygons(
          p       = proxy,
          pi      = pi,
          domain  = map_value_range(),
          palette = pal_name,
          legend_position = input$map_legend_position %||% "bottomright"
        )
      })
      
      output$region_map <- renderLeaflet({
        validate(need(im$posterior, "Load data and run model first"))
        region_map()
      })
      
      # Helper function to get time series
      
      time_series_raw <- function(dcl,id, type=c("Counts", "Proportion")) {
        type = match.arg(type)
        reg_col = dcl[["region_column"]]
        d = dcl[["date_column"]]
        y = dcl[["numerator_column"]]
        den = dcl[["denominator_column"]]
        
        if(type == "Counts") {
          ts <- dcl$data[x == id,.(d,v), env=list(x=reg_col, d=d,v=y)]
        } else {
          ts <- dcl$data[x==id, .(d,v/den), env=list(x=reg_col, d=d, v=y, den=den)]
        }
        setnames(ts, new=c("x", "y"))
        ts[!is.na(y)]
      }

      
      # observe for clicks on the counties
      
      observe({
        click <- input$region_map_shape_click
        
        # if there is no id, return
        if (is.null(click$id)) return()
        
        # get the plot data using the helper function
        plot_data <- time_series_raw(im$data_cls, id = click$id, type=input$metric_counts)
        
        # create a lightweight scatter
        p <- ggplot(plot_data, aes(x = x, y = y)) +
          geom_point(color="blue") + 
          geom_line(color="black") + 
          labs(
            title = paste("Timeseries for", click$id),
            y = input$metric_counts, x = "Date") +
          theme_minimal()

        # use popupGraph from leafpop to wrap in a list, and constrain size
        popup_html <- leafpop::popupGraph(list(p), width = 300, height = 200)
        
        # use leaflet proxy to add the html frame to the rgion map
        leafletProxy("region_map") |> 
          clearPopups() |> 
          addPopups(
            lng = click$lng,
            lat = click$lat,
            popup = popup_html,
            layerId = "timeseries_popup"
          )
      }) |> bindEvent(input$region_map_shape_click)

    }
  )
}
