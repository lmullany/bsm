# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958
label_list_pi <- list(
  scale_radio = list(
    l = "Scale",
    m = "Select the desired scale for the displayed results. Select count to display the number of visits and proportion to display the proportion of visits with the requested diagnosis out of all visits. Note that forecasts are only available when the scale matches the natural scale of the model, i.e. count for poisson and negative binomial and proportion for binomial and beta binomial."
  ),
  quantile_slider = list(
    l = "Quantile to add", 
    m = "Drag the slider to select additional quantiles to include in the results. Then click add quantile."
  )
)
button_list_pi <-list(
  add_quantile = "After selecting a quantile with the slider, add an available column with the corresponding posterior quantile for display in the table.",
  csv_button = "Download displayed data to a local csv file.",
  clear_filters = "Reset all data filters."
)


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
        radioButtons(ns("post_scale"), 
                     labeltt(label_list_pi[["scale_radio"]]),
                     choices = c("Count", "Proportion"),
                     selected = "Proportion",
                     inline = TRUE),
        sliderInput(ns("post_q_slider"), 
                    labeltt(label_list_pi[["quantile_slider"]]), 
                    min=0, max=1, step=0.01, value=0.50),
        fluidRow(column(6, 
                        add_button_hover(
                          button_list_pi[["add_quantile"]],
                          actionButton(
                            ns("post_add_q"), 
                            "Add quantile", 
                            class = "btn-primary btn-sm")
                          )
                        )
                 ),
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
            reactable::reactableOutput(ns("posterior_data"), width = "100%"),
            div(style = "display:flex; gap:10px; align-items:center;",
              add_button_hover(title = button_list_pi[["clear_filters"]],
                             actionButton(ns("clear_filters"),
                                            class = "btn-primary btn-sm",
                                            "Clear Filters")),
              add_button_hover(title = button_list_pi[["csv_button"]],
                             downloadButton(ns("download_posterior_csv"),
                                            class = BUTTON_CLASS,
                                            "Download CSV")))

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
        "0.5","0.025","0.975"
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
      
      observe({
        q <- round(input$post_q_slider %||% 0.50, 3)
        sel_qs(sort(unique(c(sel_qs(), q))))
        qname <- sub("^\\.", "0.", prettyNum(q, digits = 12, drop0trailing = TRUE))
        vis   <- visible_cols_rv() %||% default_visible
        if (!qname %in% vis) visible_cols_rv(unique(c(vis, qname)))
      }) |> bindEvent(input$post_add_q)
      
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
        out$countyfips <- as.factor(out$countyfips)
        out$region <- as.factor(out$region)
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
      
      observe({
        visible_cols_rv(input$visible_cols %||% character())
      })|>bindEvent(list(input$visible_cols, input$hidden_cols), ignoreInit = TRUE)
      
      # clear filters on button click
      observe({
        session$sendCustomMessage(
          "clear-reactable-filters",
          list(id = table_id)
        )
      }) |> bindEvent(input$clear_filters, ignoreInit = TRUE)
      
      
      # Add posterior data table reactable
      table_id <- session$ns("posterior_data")
      output$posterior_data <- reactable::renderReactable({
        
        req(input$dt_digits)
        df <- posterior_tbl(); req(df)
        
        # filter to default columns only
        keep <- visible_cols_rv() %||% intersect(default_visible, names(df))
        keep <- intersect(keep, names(df))
        if (length(keep)) df <- df[, ..keep]
        
        # clean up table names
        display_names <- map_table_names_to_display(names(df))
        if (is.null(names(display_names))) {
          names(display_names) <- names(df)
        }
        
        # Get the columns to round
        cols_to_round <- non_integer_cols_to_round(df)
        # check the digits requested
        digits   <- max(0, min(10, as.integer(input$dt_digits %||% 2)))
        
        # get column types 
        num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
        date_cols <- names(df)[sapply(df, inherits, "Date")]
        
        # define column filters
        col_defs <- lapply(names(df), function(col) {
          label <- display_names[[col]]; if (is.null(label)) label <- col
          
          is_num <- is.numeric(df[[col]])
          is_rounded <- col %in% names(cols_to_round)
          is_date <- col %in% date_cols
          if (is_num) {
            reactable::colDef(
              name        = label,
              align       = "right",
              filterable  = TRUE,
              filterMethod = numeric_range_filter_method,
              filterInput = function (values, name){ numeric_range_filter_input(values, name, table_id ) },
              format      = if (is_rounded) reactable::colFormat(digits = digits) else NULL,
            )
            
          } else if (is_date){
            reactable::colDef(
              name        = label,
              filterable  = TRUE,
              filterMethod = date_filter_method,
              filterInput = function (values, name){ date_filter_input(values, name, table_id) },
            )
            
          } else {
            reactable::colDef(
              name       = label,
              filterable = TRUE,
              filterMethod = checkbox_filter_method,
              filterInput = function (values, name){ checkbox_filter_input(values, name, table_id) },
            )
          }
        })
        names(col_defs) <- names(df)
        
        # set table height
        n <- nrow(df)
        page_size <- min(n, 10L)
        
        tbl <- reactable::reactable(
          df,
          columns = col_defs,
          defaultPageSize = page_size,
          pageSizeOptions = c(5, 10, 15, 25, 50, 100),
          searchable      = TRUE,
          filterable      = TRUE,
          highlight       = TRUE,
          striped         = TRUE,
          bordered        = TRUE,
          resizable       = TRUE,
          wrap            = FALSE,
          defaultColDef   = reactable::colDef(minWidth = 100),
          fullWidth       = TRUE,
          theme = BS_REACTABLE_THEME
        )

        # return
        tbl
      })
      
      # define download functionality
      output$download_posterior_csv <- downloadHandler(
        filename = function() {
          # Recreate the JS logic in R
          v <- input$post_scale %||% "Props"
          suffix <- if (identical(v, "Count")) "count" else "props"
          paste0("posterior_data_", suffix, ".csv")
        },
        content = function(file) {
          # Start from the same base data as the table
          df <- posterior_tbl(); req(df)
          
          # Apply the same visible columns logic you use for the table
          keep <- visible_cols_rv() %||% intersect(default_visible, names(df))
          keep <- intersect(keep, names(df))
          if (length(keep)) df <- df[, ..keep]
          
          # Now write that to CSV
          write.csv(df, file, row.names = FALSE)
        }
      )
      
      

    }
  )
}
