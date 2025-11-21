# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958
viz_posterior_ui <- function(id) {
  ns <- NS(id)
  
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
          tags$style(HTML(sprintf("
            #%s .dataTables_scrollBody thead,
            #%s .dataTables_scrollBody thead * {
              visibility: hidden !important;
              pointer-events: none !important;
            }
          ", ns("posterior_wrap"), ns("posterior_wrap")))),
          div(
            id = ns("posterior_wrap"),
            style = "width: 100%;",
            DTOutput(ns("posterior_data"), width = "100%")
          )
        )
      )
    )
  )
}

viz_posterior_server <- function(id, im, results) {
  moduleServer(
    id,
    function(input, output, session) {
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
        
        # Get the columns to round
        cols_to_round <- non_integer_cols_to_round(df)
        
        # check the digits requested
        digits   <- max(0, min(10, as.integer(input$dt_digits %||% 2)))
        
        # format the table
        tbl <- tbl |> DT::formatRound(cols_to_round, digits= digits)
        
        # return
        tbl
      })
      

    }
  )
}
