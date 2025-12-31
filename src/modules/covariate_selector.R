add_covariate_loader <- function(
    input,
    output,
    session,
    get_base_dt,
    set_base_dt,
    button_id = "cov_button_ui",
    button_label = "Add covariates",
    base_fips_col = "countyfips"
) {
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  ns <- session$ns
  
  cov_dt       <- shiny::reactiveVal(NULL)
  cov_filtered <- shiny::reactiveVal(FALSE)
  
  # UI state: show/hide imputation controls
  show_impute_ui <- shiny::reactiveVal(FALSE)
  
  # cache adjacency matrix (loaded only when needed)
  adj_mat_val <- shiny::reactiveVal(NULL)
  
  # ---- Helpers ----
  normalize_fips <- function(x) {
    x <- sprintf("%05s", as.character(x))
    gsub(" ", "0", x, fixed = TRUE)
  }
  
  show_missing_warnings <- function(dt, cols) {
    cols <- intersect(cols, names(dt))
    if (length(cols) == 0) return(invisible(NULL))
    
    na_counts <- vapply(cols, function(nm) sum(is.na(dt[[nm]])), integer(1))
    na_counts <- na_counts[na_counts > 0]
    if (length(na_counts) == 0) return(invisible(NULL))
    
    for (nm in names(na_counts)) {
      shiny::showNotification(
        paste0("Warning! missing ", na_counts[[nm]], " values found in column ", nm, "."),
        type = "warning",
        duration = NULL
      )
    }
    
    invisible(NULL)
  }
  
  warn_categorical <- function(dt, cols) {
    cols <- intersect(cols, names(dt))
    bad <- cols[!vapply(cols, function(nm) is.numeric(dt[[nm]]), logical(1))]
    if (length(bad) > 0) {
      shiny::showNotification(
        paste0(
          "Warning: categorical/non-numeric columns cannot be imputed and will be skipped: ",
          paste(bad, collapse = ", ")
        ),
        type = "warning",
        duration = NULL
      )
    }
    invisible(bad)
  }
  
  get_adj_mat <- function() {
    m <- adj_mat_val()
    if (!is.null(m)) return(m)
    m <- load_adj_matrix(PHYS_ADJ_MATRIX)
    adj_mat_val(m)
    m
  }
  
  impute_selected <- function(dt, fips_col, feature_cols, method) {
    dt <- data.table::copy(dt)
    
    feature_cols <- intersect(feature_cols, names(dt))
    if (length(feature_cols) == 0) return(dt)
    
    # skip non-numeric with warning
    nonnum <- feature_cols[!vapply(feature_cols, function(nm) is.numeric(dt[[nm]]), logical(1))]
    if (length(nonnum) > 0) {
      shiny::showNotification(
        paste0(
          "Warning: categorical/non-numeric columns cannot be imputed and will be skipped: ",
          paste(nonnum, collapse = ", ")
        ),
        type = "warning",
        duration = NULL
      )
    }
    feature_cols <- setdiff(feature_cols, nonnum)
    if (length(feature_cols) == 0) return(dt)
    
    dt[, (fips_col) := normalize_fips(get(fips_col))]
    
    if (method %in% c("mean_overall", "median_overall")) {
      for (nm in feature_cols) {
        x <- dt[[nm]]
        fill <- if (method == "mean_overall") mean(x, na.rm = TRUE) else stats::median(x, na.rm = TRUE)
        if (is.nan(fill)) next
        idx <- which(is.na(x))
        if (length(idx) > 0) data.table::set(dt, i = idx, j = nm, value = fill)
      }
      return(dt)
    }
    
    if (method %in% c("mean_neighbors", "median_neighbors")) {
      mat <- get_adj_mat()
      
      # only neighbors that exist in current cov_dt() contribute
      geoids_present <- unique(dt[[fips_col]])
      
      data.table::setkeyv(dt, fips_col)
      
      for (nm in feature_cols) {
        miss_fips <- unique(dt[is.na(get(nm)), get(fips_col)])
        if (length(miss_fips) == 0) next
        
        for (f in miss_fips) {
          if (is.null(rownames(mat)) || !(f %in% rownames(mat))) next
          
          neigh <- get_neighbors(f, mat)
          neigh <- intersect(neigh, geoids_present)
          if (length(neigh) == 0) next
          
          vals <- dt[.(neigh), get(nm)]
          vals <- vals[!is.na(vals)]
          if (length(vals) == 0) next  # leave NA (your requirement)
          
          fill <- if (method == "mean_neighbors") mean(vals) else stats::median(vals)
          if (is.nan(fill)) next
          
          dt[.(f), (nm) := fill]
        }
      }
      return(dt)
    }
    
    dt
  }
  
  # ---- File reader ----
  read_to_dt <- function(path, name) {
    ext <- tolower(tools::file_ext(name))
    
    if (ext == "csv") {
      return(data.table::as.data.table(readr::read_csv(path, show_col_types = FALSE)))
    }
    if (ext %in% c("xlsx", "xls")) {
      return(data.table::as.data.table(readxl::read_excel(path)))
    }
    if (ext == "parquet") {
      return(data.table::as.data.table(arrow::read_parquet(path)))
    }
    stop("Unsupported file type: ", ext)
  }
  
  # ---- Open modal ----
  shiny::observe({
    shiny::req(input[[button_id]])
    cov_filtered(FALSE)
    show_impute_ui(FALSE)
    
    shiny::showModal(
      shiny::modalDialog(
        title = button_label,
        size = "l",
        easyClose = TRUE,
        
        shiny::fileInput(
          ns("cov_file"),
          "Upload covariates file (CSV, Excel, Parquet)",
          accept = c(".csv", ".xlsx", ".xls", ".parquet")
        ),
        
        shiny::uiOutput(ns("cov_fips_col_ui")),
        shiny::uiOutput(ns("cov_feature_cols_ui")),
        
        shiny::fluidRow(
          shiny::column(
            12,
            shiny::div(
              style = "margin-top: 6px; margin-bottom: 6px;",
              shiny::actionButton(ns("cov_select_all"), "Select all"),
              shiny::actionButton(ns("cov_unselect_all"), "Unselect all")
            )
          )
        ),
        
        shiny::uiOutput(ns("cov_impute_controls_ui")),
        
        shiny::tags$hr(),
        
        # --- controls above the table ---
        shiny::fluidRow(
          shiny::column(
            12,
            shiny::div(
              style = "display:flex; align-items:center; gap:10px; flex-wrap:wrap;",
              shiny::actionButton(ns("cov_filter"), "Filter"),
              shiny::actionButton(ns("cov_impute_toggle"), "Impute missing")
            )
          )
        ),
        
        # Impute method UI (appears after clicking Impute missing)
        shiny::uiOutput(ns("cov_impute_controls_ui")),
        
        shiny::div(style = "margin-top: 8px;"),
        shiny::strong("Preview:"),
        DT::DTOutput(ns("cov_preview_dt")),
        
        
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("cov_add"), "Add", class = "btn-primary")
        )
        
      )
    )
  }) |> shiny::bindEvent(input[[button_id]])
  
  # ---- Impute controls UI (hidden until toggle) ----
  output$cov_impute_controls_ui <- shiny::renderUI({
    if (!isTRUE(show_impute_ui())) return(NULL)
    
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::div(
          style = "display:flex; align-items:center; gap:12px; margin-top: 6px;",
          shiny::tags$label("Imputation method:", style = "margin:0; font-weight:600;"),
          shiny::selectInput(
            ns("cov_impute_method"),
            label = NULL,
            choices = c(
              "Mean (overall)" = "mean_overall",
              "Median (overall)" = "median_overall",
              "Mean (neighbors)" = "mean_neighbors",
              "Median (neighbors)" = "median_neighbors"
            ),
            selected = "mean_overall",
            width = "260px"
          ),
          shiny::actionButton(ns("cov_impute_run"), "Run")
        )
      )
    )
  })
  
  # ---- Toggle impute controls ----
  shiny::observe({
    shiny::req(input$cov_impute_toggle)
    show_impute_ui(!isTRUE(show_impute_ui()))
  }) |> shiny::bindEvent(input$cov_impute_toggle)
  
  # ---- Load uploaded file ----
  shiny::observe({
    shiny::req(input$cov_file)
    
    dt <- tryCatch(
      read_to_dt(input$cov_file$datapath, input$cov_file$name),
      error = function(e) {
        shiny::showNotification(paste("Read failed:", e$message), type = "error", duration = NULL)
        NULL
      }
    )
    shiny::req(dt)
    
    if (anyDuplicated(names(dt)) > 0) {
      data.table::setnames(dt, make.unique(names(dt)))
    }
    
    cov_dt(dt)
    cov_filtered(FALSE)
    
    cols <- names(dt)
    
    output$cov_fips_col_ui <- shiny::renderUI({
      shiny::selectInput(
        ns("cov_fips_col"),
        "Select the FIPS code column",
        choices = cols,
        selected = if ("countyfips" %in% cols) "countyfips" else character(0)
      )
    })
    
    output$cov_feature_cols_ui <- shiny::renderUI({
      shiny::req(cov_dt())
      
      cols2 <- names(cov_dt())
      fips_col <- input$cov_fips_col %||% ""
      feat_choices <- if (fips_col %in% cols2) setdiff(cols2, fips_col) else cols2
      
      current <- shiny::isolate(input$cov_features) %||% feat_choices
      selected <- intersect(current, feat_choices)
      
      shiny::selectizeInput(
        ns("cov_features"),
        "Select feature columns",
        choices  = feat_choices,
        selected = selected,
        multiple = TRUE,
        width    = "100%",
        options  = list(
          plugins = list("remove_button"),
          placeholder = "Type to search columns…"
        )
      )
    })
  }) |> shiny::bindEvent(input$cov_file)
  
  # ---- Keep feature choices synced when fips changes ----
  shiny::observe({
    shiny::req(cov_dt(), input$cov_fips_col)
    
    cols <- names(cov_dt())
    fips_col <- input$cov_fips_col
    shiny::req(fips_col %in% cols)
    
    feat_choices <- setdiff(cols, fips_col)
    current <- shiny::isolate(input$cov_features) %||% feat_choices
    selected <- intersect(current, feat_choices)
    
    shiny::updateSelectizeInput(
      session,
      "cov_features",
      choices = feat_choices,
      selected = selected,
      server = TRUE
    )
  }) |> shiny::bindEvent(input$cov_fips_col)
  
  # ---- Select all / unselect all ----
  shiny::observe({
    shiny::req(input$cov_select_all)
    shiny::req(cov_dt())
    
    cols <- names(cov_dt())
    fips_col <- input$cov_fips_col %||% ""
    feats <- if (fips_col %in% cols) setdiff(cols, fips_col) else cols
    
    shiny::updateSelectizeInput(session, "cov_features", selected = feats, server = TRUE)
  }) |> shiny::bindEvent(input$cov_select_all)
  
  shiny::observe({
    shiny::req(input$cov_unselect_all)
    shiny::updateSelectizeInput(session, "cov_features", selected = character(0), server = TRUE)
  }) |> shiny::bindEvent(input$cov_unselect_all)
  
  # ---- Preview ----
  preview_dt <- shiny::reactive({
    shiny::req(cov_dt())
    dt <- cov_dt()
    
    cols <- names(dt)
    fips_col <- input$cov_fips_col %||% ""
    feats <- input$cov_features %||% character(0)
    
    keep <- unique(c(if (fips_col %in% cols) fips_col else character(0), feats))
    keep <- intersect(keep, cols)
    
    dt[, keep, with = FALSE]
  })
  
  output$cov_preview_dt <- DT::renderDT({
    DT::datatable(
      preview_dt(),
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        scrollY = "45vh",
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100)
      )
    )
  }, server = TRUE)
  
  # ---- Filter ----
  shiny::observe({
    shiny::req(input$cov_filter)
    shiny::req(cov_dt(), input$cov_fips_col)
    
    base_dt0 <- get_base_dt()
    shiny::req(!is.null(base_dt0))
    shiny::req(base_fips_col %in% names(base_dt0))
    
    up_dt <- data.table::copy(cov_dt())
    fips_col <- input$cov_fips_col
    shiny::req(fips_col %in% names(up_dt))
    
    up_dt[, (fips_col) := normalize_fips(get(fips_col))]
    base_fips <- normalize_fips(base_dt0[[base_fips_col]])
    
    before <- nrow(up_dt)
    up_dt <- up_dt[get(fips_col) %in% base_fips]
    after <- nrow(up_dt)
    
    cov_dt(up_dt)
    cov_filtered(TRUE)
    
    feats_now <- input$cov_features %||% character(0)
    show_missing_warnings(up_dt, feats_now)
    
    shiny::showNotification(
      paste("Filtered:", after, "of", before, "rows kept."),
      type = "message"
    )
  }) |> shiny::bindEvent(input$cov_filter)
  
  # ---- Impute run ----
  shiny::observe({
    shiny::req(input$cov_impute_run)
    shiny::req(cov_dt(), input$cov_fips_col)
    
    dt <- data.table::copy(cov_dt())
    fips_col <- input$cov_fips_col
    shiny::req(fips_col %in% names(dt))
    
    feats <- input$cov_features %||% character(0)
    feats <- intersect(feats, names(dt))
    
    # warn about categorical and skip them
    warn_categorical(dt, feats)
    
    method <- input$cov_impute_method
    dt2 <- tryCatch(
      impute_selected(dt, fips_col = fips_col, feature_cols = feats, method = method),
      error = function(e) {
        shiny::showNotification(paste("Impute failed:", conditionMessage(e)), type = "error", duration = NULL)
        NULL
      }
    )
    shiny::req(!is.null(dt2))
    
    cov_dt(dt2)
    
    # warn about remaining missingness
    show_missing_warnings(dt2, feats)
    
    shiny::showNotification("Imputation applied (selected numeric features).", type = "message")
  }) |> shiny::bindEvent(input$cov_impute_run)
  
  # ---- Add ----
  shiny::observe({
    shiny::req(input$cov_add)
    shiny::req(cov_dt(), input$cov_fips_col)
    
    base_dt <- data.table::copy(get_base_dt())
    shiny::req(!is.null(base_dt))
    shiny::req(data.table::is.data.table(base_dt))
    shiny::req(base_fips_col %in% names(base_dt))
    
    up_dt <- data.table::copy(cov_dt())
    fips_col <- input$cov_fips_col
    shiny::req(fips_col %in% names(up_dt))
    
    feats <- input$cov_features %||% character(0)
    feats <- intersect(feats, names(up_dt))
    
    up_dt[, (fips_col) := normalize_fips(get(fips_col))]
    base_dt[, (base_fips_col) := normalize_fips(get(base_fips_col))]
    
    join_cols <- unique(c(fips_col, feats))
    join_cols <- intersect(join_cols, names(up_dt))
    join_dt <- up_dt[, join_cols, with = FALSE]
    
    if (fips_col != base_fips_col) {
      data.table::setnames(join_dt, fips_col, base_fips_col)
    }
    
    base_dt[, .__row_id__ := .I]
    
    merged <- merge(
      base_dt,
      join_dt,
      by = base_fips_col,
      all.x = TRUE,
      sort = FALSE,
      allow.cartesian = TRUE
    )
    
    data.table::setorder(merged, .__row_id__)
    merged[, .__row_id__ := NULL]
    
    new_cols <- setdiff(names(merged), names(base_dt))
    show_missing_warnings(merged, new_cols)
    
    set_base_dt(merged)
    shiny::removeModal()
    shiny::showNotification("Covariates added to base table.", type = "message")
  }) |> shiny::bindEvent(input$cov_add)
}
