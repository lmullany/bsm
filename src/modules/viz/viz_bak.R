# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

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
            
            div(
              "Date Selection",
              style = "font-weight: 600; font-size: 0.95rem; margin-bottom: 6px;"
            ),
            card(
              style = "width: 100%; height:100px;",
              class = "mb-1",
              card_body(
                style = "height:100%; padding: 0;",
                plotlyOutput(ns("date_spark"), height = "100%", width = "100%")
              )
            ), # Placeholder dates to initialize
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
            
            radioButtons(
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
            # conditionalPanel(
            #   condition = "input.map_metric == 'exceedance'",
            #   uiOutput(outputId = ns("metric_exceedance_thresh_ui")),
            #   ns = ns
            # ),
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
      ),
      nav_panel(
        title="Posterior Data",
        layout_sidebar(
          sidebar = sidebar(
            id = ns("posterior_sidebar"),
            width = SIDEBAR_WIDTH*2,
            open = "open",
            title = "Options",
            radioButtons(ns("post_scale"), "Scale",
                         choices = c("Count", "Proportion"),
                         selected = "Proportion",
                         inline = TRUE),
            sliderInput(ns("post_q_slider"), "Quantile to add", min=0, max=1, step=0.01, value=0.50),
            fluidRow(column(6, actionButton(ns("post_add_q"), "Add quantile", class = "btn-primary btn-sm"))),
            tags$hr(),
            uiOutput(ns("post_col_picker")),
            numericInput(ns("dt_digits"), label = "Table decimals", value=2, min=0, max=10, step=1)
          ),
          card(
            card_body(
              style = "overflow: visible;",
              div(
                id = ns("posterior_wrap"),
                style = "width: 100%;",
                DTOutput(ns("posterior_data"), width = "100%")
              )
            )
          )
        ),
        navbar_options = list(class = "bg-primary", theme = "dark", underline=FALSE)
      )
    )
  )
  
}

viz_server <- function(id, dc, im, results) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns = session$ns

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
      # plots <- reactive({
      #   req(im$posterior)
      #   make_timeseries_plots(res_data = im$posterior, date_col = "date", use_prop = TRUE, F, F, F)
      # })
      
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
      
      
      # Download posterior data
      output$download_posterior_btn <- downloadHandler(
        filename = "posterior_data.csv" , content = \(file) data.table::fwrite(im$posterior, file)
      )
      

      default_visible <- c(
        "countyfips","date","region","target","overall",
        "predicted_median","predicted_lower","predicted_upper"
      )
      
      `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
      quant_cols <- function(nm) grep("^(0(\\.\\d+)?|1)$", nm, value = TRUE)
      build_probs <- function(sel_qs_vec) { # precompute columns
        base <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
        user <- round(sel_qs_vec %||% 0.50, 3)
        probs <- sort(unique(round(c(base, user), 3)))
        pmin(pmax(probs, 0), 1)
      }
      
      visible_cols_rv <- reactiveVal(NULL)
      sel_qs <- reactiveVal(0.50)
      
      observeEvent(input$post_add_q, {
        q <- round(input$post_q_slider %||% 0.50, 3)
        sel_qs(sort(unique(c(sel_qs(), q))))
        qname <- sub("^\\.", "0.", prettyNum(q, digits = 12, drop0trailing = TRUE))
        vis   <- visible_cols_rv() %||% default_visible
        if (!qname %in% vis) visible_cols_rv(unique(c(vis, qname)))
      })
      
      all_probs <- reactive({
        qs <- tryCatch(sel_qs(), error = function(e) 0.50)
        build_probs(qs)
      })
      
      posterior_qdf <- reactive({
        req(im$model, im$data_cls)
        qdf <- get_posterior_quantiles(
          im$model, im$data_cls,
          probs = all_probs(),
          use_count_scale = isTRUE(input$post_scale == "Count")
        )
        data.table::setDT(qdf)
        
        qdf[, countyfips := as.character(countyfips)]
        if (!inherits(qdf$date, "Date")) qdf[, date := as.Date(date)]
        
        rn <- names(qdf)
        new <- sub("^(props_|counts?_)","", rn) 
        is_numlike <- grepl("^\\d*\\.?\\d+$", new)
        if (any(is_numlike)) {
          new[is_numlike] <- sub(
            "^\\.", "0.",
            prettyNum(as.numeric(new[is_numlike]), digits = 12, drop0trailing = TRUE)
          )
        }
        data.table::setnames(qdf, rn, new, skip_absent = TRUE)
        
        keep <- intersect(c("countyfips","date", quant_cols(names(qdf))), names(qdf))
        qdf[, ..keep]
      }) |> bindEvent(all_probs(), input$post_scale)
      
      posterior_tbl <- reactive({
        req(im$posterior)
        base <- data.table::as.data.table(im$posterior)
        base[, countyfips := as.character(countyfips)]
        if (!inherits(base$date, "Date")) base[, date := as.Date(date)]
        
        qdf    <- posterior_qdf()
        q_only <- setdiff(names(qdf), c("countyfips","date"))
        
        out <- merge(
          base,
          qdf[, c("countyfips","date", q_only), with = FALSE],
          by = c("countyfips","date"),
          all.x = TRUE, sort = FALSE
        )
        nm <- names(out)
        has_xy <- grepl("\\.(x|y)$", nm)
        if (any(has_xy)) {
          bare <- sub("\\.(x|y)$", "", nm)
          for (b in unique(bare[grepl("\\.x$", nm)])) {
            x <- paste0(b, ".x"); y <- paste0(b, ".y")
            if (y %in% nm) out[, (y) := NULL]
            if (x %in% names(out)) data.table::setnames(out, old = x, new = b)
          }
        }
        # ensure id columns are leading to start
        id_first <- intersect(c("countyfips","date","region"), names(out)) 
        qcols    <- quant_cols(names(out))
        others   <- setdiff(names(out), c(id_first, qcols))
        data.table::setcolorder(out, c(id_first, others, qcols))
        out[]
      })
      
      output$post_col_picker <- renderUI({
        df <- posterior_tbl(); req(df)
        cols_now <- names(df)
        
        vis <- visible_cols_rv()
        if (is.null(vis)) vis <- intersect(default_visible, cols_now)
        vis    <- intersect(vis, cols_now)
        hidden <- setdiff(cols_now, vis)
        
        tags$div(
          style = "display:flex; gap:12px; align-items:flex-start;",
          tags$div(
            style = "flex:1;",
            tags$label("Hidden"),
            tags$select(
              id = session$ns("hidden_select"), multiple = "multiple", size = 14, style = "width:100%;",
              lapply(hidden, function(x) tags$option(value = x, x))
            )
          ),
          tags$div(
            style = "flex:1;",
            tags$label("Visible"),
            tags$select(
              id = session$ns("visible_select"), multiple = "multiple", size = 14, style = "width:100%;",
              lapply(vis, function(x) tags$option(value = x, x))
            )
          ),
          # UI for hidden/selected selection lists
          tags$script(HTML(sprintf("
                  (function(){
                    var visId = '%s', hidId = '%s', visKey = '%s', hidKey = '%s';
                    function el(id){ return document.getElementById(id); }
                    function values(sel){ return Array.from(sel.options).map(function(o){return o.value;}); }
                    function sync(){
                      if (window.Shiny) {
                        Shiny.setInputValue(visKey, values(el(visId)), {priority:'event'});
                        Shiny.setInputValue(hidKey, values(el(hidId)), {priority:'event'});
                      }
                    }
                    function moveSelected(fromSel, toSel){
                      Array.from(fromSel.selectedOptions).forEach(function(opt){ toSel.appendChild(opt); });
                      sync();
                    }
                    var v = el(visId), h = el(hidId);
                    if(!v || !h) return;
                    v.addEventListener('dblclick', function(){ moveSelected(v, h); });
                    h.addEventListener('dblclick', function(){ moveSelected(h, v); });
                  })();
                ",
             session$ns("visible_select"),
             session$ns("hidden_select"),
             session$ns("visible_cols"),
             session$ns("hidden_cols")
          )))
        )
      })
      
      observeEvent(list(input$visible_cols, input$hidden_cols), {
        visible_cols_rv(input$visible_cols %||% character())
      }, ignoreInit = TRUE)
      
      output$posterior_data <- renderDT({
        req(input$dt_digits)
        df <- posterior_tbl(); req(df)
        
        keep <- visible_cols_rv() %||% intersect(default_visible, names(df))
        keep <- intersect(keep, names(df))
        if (length(keep)) df <- df[, ..keep]
        # CSV export logs _props or _count
        csv_btn <- list(
          extend = "csvHtml5",
          text = "Download CSV",
          title = NULL,
          filename = DT::JS(sprintf(
            "function(){
                var id = '%s';
                var v = (window.Shiny && Shiny.getInputValue) ? Shiny.getInputValue(id)
                        : (document.querySelector('input[name=\"' + id + '\"]:checked') || {}).value;
                var suffix = (v === 'Count') ? 'count' : 'props';
                return 'posterior_data_' + suffix;
              }",
            session$ns("post_scale")
          )),
          exportOptions = list(columns = ":visible")
        )
        tbl <- DT::datatable(
          df,
          options = list(
            autoWidth   = TRUE,
            deferRender = TRUE,
            scrollX     = TRUE,
            pageLength  = 5,
            lengthMenu  = list(c(5,10,15,25,50,100,-1), c("5","10","15","25","50","100","All")),
            dom = '<"d-flex justify-content-between align-items-center mb-2"Bi><"mb-2"f>t<"mt-2 d-flex justify-content-between align-items-center"lp>',
            buttons     = list(csv_btn)
          ),
          rownames = FALSE, filter = "top", extensions = c("Buttons"),
          colnames = map_table_names_to_display(names(df)),
          width = "100%"
        )
        cols_to_round <- non_integer_cols_to_round(df)
        #is_num <- vapply(df, function(x) is.numeric(x) || inherits(x, "integer64"), logical(1))
        #num_cols <- names(df)[is_num]
        digits   <- max(0, min(10, as.integer(input$dt_digits %||% 2)))
        # if (length(num_cols)) {
        #   tbl <- DT::formatRound(tbl, columns = num_cols, digits = digits)
        # }
        tbl <- tbl |> DT::formatRound(cols_to_round, digits= digits)
        tbl
      })
    }
  )
}
