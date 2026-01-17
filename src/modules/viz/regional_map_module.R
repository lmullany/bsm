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
    l = "Date Selected:",
    m = paste("
    The red vertical line can be used to select the date to display on the map. The 
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
          style = "font-weight: 600; font-size: 0.95rem; margin-bottom: 6px; display:flex; gap:8px; align-items:baseline;",
          div(labeltt(label_list_rm[["date_sparkline"]])),
          htmlOutput(ns("target_date_lbl"))
        ),
        spark_card, 
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
      last_observed_date <- reactive({
        d <- all_dates()
        req(length(d) > 0)
        k <- im$nforecasts %||% 0
        idx <- max(1, length(d) - k)
        as.Date(d[idx])
      })
      
      all_dates <- reactive({
        req(im$data_cls)
        as.Date(sort(unique(im$data_cls$data[[im$data_cls$date_col]])))
      })
      

      # hold the date of the red sliding  line!      
      cursor_date <- reactiveVal(NULL)
                                 
      target_date <- reactive({
        d <- all_dates()
        req(length(d) > 0)
        
        cd <- cursor_date()
        if (is.null(cd)) return(last_observed_date())
        
        # constrain the target date to snap to the nearest
        d[which.min(abs(d - cd))]
      })
      
      # when the target date change, the label for the Selected Date (above)
      # the plot should also update
      output$target_date_lbl <- renderUI({
        req(target_date())
        tags$span(
          sprintf("(%s)", format(target_date(), "%Y-%m-%d"))
        )
      })
        
      
      # observe the red date selector
      observe({
        
        # get the slide event
        r <- plotly::event_data("plotly_relayout", source = "date_spark_src", priority = "event")
        
        x0 <- r[["shapes[0].x0"]]; x1 <- r[["shapes[0].x1"]]
        
        # if this is null, just return
        if (is.null(x0) && is.null(x1)) return()
        
        # plotly returns as character so lets convert to a number
        # and datek the average of these
        t0 <- as.POSIXct(x0, tz = "UTC")
        t1 <- as.POSIXct(x1, tz = "UTC")
        x_new <- as.Date(as.POSIXct(mean(as.numeric(c(t0, t1)), na.rm = TRUE), origin = "1970-01-01", tz = "UTC"))
        
        # check all dates, and make sure that the new date is constrained to that range and snapped to an 
        # existing date
        d <- all_dates()
        x_new <- min(max(x_new, min(d)), max(d))
        x_snap <- d[which.min(abs(d - x_new))]
        
        # store the date of the red line
        if (identical(cursor_date(), x_snap)) return()
        cursor_date(x_snap)
        
        # relayout the plot so that it moves the red line to the new location
        plotly::plotlyProxy("date_spark", session) |>
          plotly::plotlyProxyInvoke(
            "relayout",
            list(shapes = list(vertical_date_line(x_snap)))
          )
      }) |> bindEvent(input$date_spark_relayout,ignoreInit = TRUE)
      
      get_region_wide_series <- function(dcl, type, carry_forward=0) {

        date_col <- dcl$date_col
        num_col <- dcl$numerator_col
        den_col <- dcl$denominator_col
        
        dt <- data.table::as.data.table(dcl$data)
        if (!(date_col %in% names(dt))) return(NULL)
        
        # get the spark series, depending on counts/proportion        
        if(type == "Counts") {
          series <- dt[, .(total = sum(x, na.rm = TRUE)), by = c(date_col), env=list(x=num_col)]
        } else {
          series <- dt[, .(total = sum(x, na.rm = TRUE)/sum(y, na.rm=TRUE)), by = c(date_col), env=list(x=num_col, y=den_col)]
        }
        data.table::setnames(series, date_col, "date")
        
        # make sure we are ordered by date!
        setorderv(series, "date")
        
        # Now, we need to set the n_forecast last dates to the last known data point
        if(carry_forward>0) {
          series[(.N-carry_forward+1):.N, total:=series[.N-carry_forward, total]]
        }
        
        series
        
      }
      
      spark_series <- reactive({
        req(im$data_cls)
        get_region_wide_series(
          dcl = im$data_cls, 
          type = input$metric_counts,
          carry_forward = im$nforecasts
        )
      }) |> bindEvent(input$metric_counts)
      
      output$date_spark <- renderPlotly({
        
        # require the region wide series
        req(spark_series())
        
        # generate the plot
        p <- region_wide_time_series_plot(
          spark_series(),
          init_date = isolate(cursor_date() %||% last_observed_date()),
          type = im$metric_counts, 
          forecasts = im$nforecasts
        )
           
        # register the even
        p <- htmlwidgets::onRender(
          p, 
          sprintf(
            "function(el, x) {
              el.on('plotly_relayout', function(e) {
                if (window.Shiny) Shiny.setInputValue('%s', e, {priority: 'event'});
              });
            }",
            ns("date_spark_relayout")
          ))

        p
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
          palette = input$map_palette %||% "viridis",
          legend_position = input$map_legend_position %||% "bottom_right"
        ) |>
          leaflet.extras::addFullscreenControl() |>
          leaflet.extras::addResetMapButton()
      })
      
      output$region_map <- renderLeaflet({
        validate(need(im$posterior, "Load data and run model first"))
        region_map() |> enable_draggable_legend()
      })
      
      # Helper function to get time series
      
      time_series_raw <- function(dcl,id, type=c("Counts", "Proportion")) {
        type = match.arg(type)
        reg_col = dcl[["region_column"]]
        d = dcl[["date_column"]]
        y = dcl[["numerator_column"]]
        den = dcl[["denominator_column"]]
        
        if(type == "Counts") {
          ts <- dcl$data[x == id,.(region, d,v), env=list(x=reg_col, d=d,v=y)]
        } else {
          ts <- dcl$data[x==id, .(region,d,v/den), env=list(x=reg_col, d=d, v=y, den=den)]
        }
        setnames(ts, new=c("region", "x", "y"))
        list(ts = ts[!is.na(y)], label = ts[1,region])
      }

      
      # observe for clicks on the counties
      
      observe({
        click <- input$region_map_shape_click
        
        # if there is no id, return
        if (is.null(click$id)) return()
        
        # get the plot data using the helper function
        plot_data <- time_series_raw(im$data_cls, id = click$id, type=input$metric_counts)
        
        # create a lightweight scatter
        p <- time_series_popup(
          ts = plot_data[["ts"]],
          ts_label = plot_data[["label"]],
          ts_type = input$metric_counts,
          v_date = target_date()
        )

        # use popupGraph from leafpop to wrap in a list, and constrain size
        popup_html <- leafpop::popupGraph(list(p), type = "png", width = 300, height = 200)
        
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


#----------PLOTS--------------------#
# LATER, WE MOVE THIS TO A SEPARATE FILE
#----------------------------------------#

time_series_popup <- function(
    ts,
    ts_label,
    ts_type,
    v_date,
    colors=list(
      point="blue",
      line="black",
      vline="red"
    )
) {
  p <- ggplot(ts, aes(x = x, y = y)) +
    geom_point(color=colors[["point"]]) + 
    geom_line(color=colors[["line"]]) +
    geom_vline(
      mapping = aes(xintercept = v_date),
      linetype = "dashed",
      color=colors[["vline"]]) + 
    labs(
      title = paste("Timeseries for", ts_label),
      y = ts_type,
      x="",
      caption = paste0("Selected Date: ", v_date)
    ) + 
    theme_minimal() + 
    theme(plot.caption = element_text(color=colors[["vline"]], size=8))
  
  p
  
}


region_wide_time_series_plot <- function(
    series,
    init_date,
    type,
    forecasts=0,
    colors = list(
      line = "#636EFA",
      draggable_line = "red"
    )
) {

  # get the yaxis title, based on the type
  yaxis_title = fifelse(type=="Counts", "Total Cases", "Percent of ED Visits") 
  
  # generate the plot
  p <- plotly::plot_ly(
    source = "date_spark_src"
  )
  
  if(forecasts>0) {
    # add the forecast dates
    p <- p |> add_trace(
      data = series[(.N-forecasts):.N],
      x = ~date, y = ~total,
      type = "scatter", mode = "lines+markers",
      line = list(color = colors[["line"]], dash='dash'),
      marker = list(color=colors[["line"]])
    )
  }
  # add the non-forecast dates
  p <- p |>  add_trace(
    data = series[1:(.N-forecasts)],
    x = ~date, y = ~total,
    type = "scatter", mode = "lines+markers",
    line = list(color = colors[["line"]]),
    marker = list(color=colors[["line"]])
  )
  
  # add the layout and main configuration
  p <- p |> plotly::layout(
      margin = list(l = 28, r = 6, t = 4, b = 22),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      xaxis = list(
        title = list(text = "", font = list(color=colors[["line"]], size=10)),
        showticklabels = FALSE, ticks = "", showgrid = FALSE, zeroline = FALSE, 
        # fix the x range
        fixedrange=TRUE
      ),
      yaxis = list(
        title = list(text = yaxis_title, font=list(color=colors[["line"]], size=10)),
        showticklabels = FALSE, ticks = "", showgrid = FALSE, zeroline = FALSE,
        # fix the y range
        fixedrange = TRUE
      ),
      showlegend = FALSE,
      
      shapes = list(
        vertical_date_line(
          init_date,
          color=colors[["draggable_line"]]
        )
      )
    ) |> 
    plotly::config(
      displayModeBar = FALSE,
      edits = list(shapePosition = TRUE),
      scrollZoom = FALSE,
      doubleClick = FALSE
    )

  p
}

# Helper to make the spark shape
vertical_date_line <- function(cursor_date, color="red") {
  
  vertical_line <- list(
    type = "line", xref = "x", yref = "paper",
    x0 = cursor_date, x1 = cursor_date, y0 = 0, y1 = 1,
    line = list(color = color, width = 3, dash = "dash")
  )

}


