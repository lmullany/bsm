# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

add_feature_ui <- function(id) {
  ns <- NS(id)
  feature_id <- ns("feature")
  nav_panel(
    title = "Add Feature",
    value = "add_feature",
    tags$style(HTML(sprintf("
      #%s .bslib-sidebar-layout > .sidebar {
        transition: width 150ms ease, padding 150ms ease, margin 150ms ease;
      }
      #%s.sidebar-collapsed .bslib-sidebar-layout > .sidebar {
        width: 0;
        padding: 0;
        margin: 0;
        overflow: hidden;
      }
      #%s.sidebar-collapsed .bslib-sidebar-layout > .main {
        width: 100%%;
      }
    ", ns("viz_wrap"), ns("viz_wrap"), ns("viz_wrap")))) ,
    div(
      id = ns("viz_wrap"),
      # Add feature LHS collapsible panel
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          id    = ns("config_sidebar"),
          width = SIDEBAR_WIDTH * 2,
          div(
            style = "display:flex; align-items:center; justify-content:space-between; gap:.5rem;",
            strong("Controls / Filters"),
            actionButton(ns("toggle_sidebar"), "Hide", class = "btn btn-sm btn-outline-secondary")
          ),
          # Dropdown tied to conditional panels
          selectInput(
            inputId  = ns("feature"),
            label    = "Select Feature Type",
            choices  = c(
              "Mean"                   = "mean",
              "Quantile"               = "quantile",
              "Confidence Interval"    = "confidence_interval",
              "Exceedance Probability" = "exceedance_probability"
            ),
            selected = "mean"
          ),
          # Scale selection
          radioButtons(
            inputId  = ns("feature_scale"),
            label    = "Scale",
            choices  = c("Counts" = "counts", "Proportion" = "proportion"),
            selected = "counts",
            inline   = TRUE
          ),
          
          # Conditional panels
          conditionalPanel(
            condition = sprintf("input['%s'] == 'quantile'", feature_id),
            numericInput(ns("q_val"), "Quantile (0–1)", value = 0.20, min = 0, max = 1, step = 0.01)
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'confidence_interval'", feature_id),
            numericInput(ns("ci_val"), "Confidence level (0–1)", value = 0.90, min = 0, max = 1, step = 0.01)
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'exceedance_probability'", feature_id),
            numericInput(ns("exceed_val"), "Exceedance threshold", value = 0, min = 0, step = 0.01)
          ),
          # Free text fields
          checkboxInput(ns("auto_labels"), "Auto-generate name/description", value = TRUE),
          textInput(ns("feature_name"), "Feature name", value = ""),
          textInput(ns("feature_desc"), "Feature description", value = ""),
          # Add feature + select feature(s)
          add_button_hover(
            title = "Add the selected feature to the table",
            actionButton(ns("add_feature"), "Add Feature", class = "btn-primary btn-sm")
          ),
          tags$hr(),
          selectizeInput(
            inputId  = ns("display_cols"),
            label    = "Displayed columns",
            choices  = character(0),
            selected = character(0),
            multiple = TRUE,
            options  = list(
              placeholder = "Select columns to display…",
              plugins = list("remove_button")
            )
          ),
          add_button_hover(
            title = "Open delete feature window",
            actionButton(ns("delete_feature"), "Open delete feature window", class = "btn-primary btn-sm")
          ),
          # Decimal selection
          numericInput(ns("dt_digits"), "Table decimals", value = 2, min = 0, max = 10, step = 1)
        ),
        # Reactable Table RHS
        bslib::card(
          bslib::card_header(div(
            style = "display:flex; align-items:center; justify-content:space-between; width:100%;",
            span("Posterior Data")
          ),
          class = "bg-primary"),
          bslib::card_body(
            reactable::reactableOutput(ns("posterior_data"), width = "100%"),
            div(style="margin-top:10px;",
                downloadButton(ns("download_posterior_csv"), "Download CSV", class = "btn-primary btn-sm")
            )
          )
        )
        
      )
    )
  )
}


add_feature_server <- function(id, dc = NULL, im = NULL, results = NULL) {
  moduleServer(id, function(input, output, session) {
    options(shiny.fullstacktrace = TRUE)
    ns <- session$ns
    
    # Sidebar open/close
    sidebar_open <- reactiveVal(TRUE)
    observe({
      sidebar_open(!sidebar_open())
      shinyjs::toggleClass(ns("viz_wrap"), "sidebar-collapsed", !sidebar_open())
      updateActionButton(session, "toggle_sidebar", label = if (sidebar_open()) "Hide" else "Show")
    })|> bindEvent(input$toggle_sidebar, ignoreInit = TRUE)
    
    # Add features
    rv <- reactiveValues(
      next_id  = 0L,
      features = list(),
      order    = character(0),
      last_id  = NULL,
      probs    = 0.5,
      refresh  = 0L,
      force_select_fid = NULL
    )
    
    feature_choice_map <- function() {
      if (!length(rv$order)) return(setNames(character(0), character(0)))
      setNames(
        rv$order,
        vapply(rv$order, function(fid) rv$features[[fid]]$label %||% fid, character(1))
      )
    }
    
    new_id <- function() {
      rv$next_id <- rv$next_id + 1L
      paste0("feat_", rv$next_id)
    }
    
    # default feature name/description
    default_feature_spec <- reactive({
      ft <- input$feature
      ft_name <- switch(
        ft,
        mean = sprintf("Mean (%s)", 
                       input$feature_scale),
        quantile = sprintf("Quantile q=%s (%s)", 
                           fmt_num(input$q_val %||% 0.50),
                           input$feature_scale),
        confidence_interval = sprintf("%s CI (%s)", 
                                      fmt_num(input$ci_val %||% 0.90),
                                      input$feature_scale),
        exceedance_probability = sprintf("Exceedance probability with threshold %s (%s)",
                                         fmt_num(input$exceed_val %||% 0.0),
                                         input$feature_scale),
        "Feature"
      )
    
      ft_desc <- switch(
          ft,
          mean = sprintf("Posterior mean of the fitted distribution on the %s scale.",
                         input$feature_scale),
          quantile = sprintf("Posterior quantile at q=%s on the %s scale.", 
                             fmt_num(input$q_val %||% 0.50),
                             input$feature_scale),
          confidence_interval = sprintf("%s%% posterior credible interval on the %s scale.", 
                                        fmt_num(100 * (input$ci_val %||% 0.90)),
                                        input$feature_scale),
          exceedance_probability = sprintf(
                            "Posterior probability that the value exceeds %s on the %s scale.",
                            fmt_num(input$exceed_val %||% 0.0),
                            input$feature_scale
                            ),
          ""
      )
    list(name = ft_name, desc = ft_desc)
    })
      
    last_auto <- reactiveValues(name = "", desc = "")
    
    observe({
        req(input$auto_labels)
        if (!isTRUE(input$auto_labels)) return()
        
        spec <- default_feature_spec()
        updateTextInput(session, "feature_name", value = spec$name)
        updateTextInput(session, "feature_desc", value = spec$desc)
      }
    )|>
      bindEvent(list(input$feature, input$q_val, input$ci_val, input$exceed_val, input$feature_scale),
                ignoreInit = FALSE)
    
    observe({
      # When switching auto back on, immediately sync to current params
      if (isTRUE(input$auto_labels)) {
        spec <- default_feature_spec()
        updateTextInput(session, "feature_name", value = spec$name)
        updateTextInput(session, "feature_desc", value = spec$desc)
      }
    }) |> bindEvent(input$auto_labels) 
    
    # get values for inputed quantile level/CI level
    need_probs <- function(ft) {
      if (ft == "quantile") return(round(input$q_val %||% 0.50, 3))
      if (ft == "confidence_interval") {
        lvl <- input$ci_val %||% 0.90
        a <- (1 - lvl) / 2
        return(round(c(a, 1 - a), 3))
      }
      numeric(0)
    }
    # all quantiles collected together
    all_probs <- reactive({
      p <- sort(unique(c(0.5, rv$probs)))
      pmin(pmax(p, 0), 1)
    })
    
    # Cache for features
    feature_cache <- reactiveVal(list())
    cache_key <- function(...) paste(..., sep = "||")
    cache_get <- function(k) feature_cache()[[k]]
    cache_set <- function(k, v) {
      x <- feature_cache(); x[[k]] <- v; feature_cache(x)
    }
    cache_clear <- function() feature_cache(list())
    observe({
      cache_clear()
    }) |> bindEvent(list(im$model, im$data_cls), ignoreInit = TRUE)
    
    # Feature
    observe({
      ft <- input$feature
      sc <- input$feature_scale
      
      spec <- default_feature_spec()
      nm <- trimws(input$feature_name %||% "")
      ds <- trimws(input$feature_desc %||% "")
      if (!nzchar(nm)) nm <- spec$name
      if (!nzchar(ds)) ds <- spec$desc
      
      # --- gather reserved underlying names (existing) ---
      reserved_names <- character(0)
      if (length(rv$order)) {
        reserved_names <- unlist(lapply(rv$order, function(fid) {
          f <- rv$features[[fid]]
          c(f$label, f$out_cols)
        }))
      }
      reserved_base_names <- core_table_colnames()
      
      reserved_under <- unique(c(reserved_names, reserved_base_names))
      
      # Map reserved underlying -> display
      reserved_disp <- unique(unname(
        map_table_names_to_display(
          reserved_under,
          quantile_suffix = NULL,
          keep_names = TRUE
        )
      ))
      
      # Universe of strings you must not collide with
      reserved_universe <- unique(c(reserved_under, reserved_disp))
      
      # --- proposed new names (underlying) ---
      new_under <- c(
        nm,
        switch(
          ft,
          confidence_interval = c(paste0(nm, " Lower"), paste0(nm, " Upper")),
          nm
        )
      )
      
      # Map proposed new underlying -> display
      new_disp <- unique(unname(
        map_table_names_to_display(
          new_under,
          quantile_suffix = NULL,
          keep_names = TRUE
        )
      ))
      
      # Block if either underlying OR display collides with anything reserved
      block <- intersect(unique(c(new_under, new_disp)), reserved_universe)
      if (length(block)) {
        showNotification(
          sprintf('A feature/column name "%s" conflicts with an existing name.', block[1]),
          type = "warning",
          duration = 6
        )
        return(NULL)
      }
      
      params <- switch(
        ft,
        mean = list(),
        quantile = list(q = input$q_val %||% 0.50),
        confidence_interval = list(ci = input$ci_val %||% 0.90),
        exceedance_probability = list(threshold = input$exceed_val %||% 0),
        list()
      )
      
      out_cols <- switch(
        ft,
        confidence_interval = c(paste0(nm, " Lower"), paste0(nm, " Upper")),
        nm
      )
      
      fid <- new_id()
      rv$features[[fid]] <- list(
        id = fid,
        label = nm,
        description = ds,
        feature_type = ft,
        feature_scale = sc,
        out_cols = out_cols,
        params = params
      )
      rv$order <- c(rv$order, fid)
      rv$last_id <- fid
      
      qs <- need_probs(ft)
      if (length(qs)) {
        rv$probs <- sort(unique(c(rv$probs, qs)))
        rv$refresh <- rv$refresh + 1L
      }
      
      choices <- setNames(
        rv$order,
        vapply(rv$order, function(x) rv$features[[x]]$label, character(1))
      )
      
      isolate({
        cur <- input$display_cols %||% character(0)
        if (length(cur) == 0) cur <- core_cols()
        updateSelectizeInput(
          session,
          "display_cols",
          choices  = ordered_choices_named(),
          selected = unique(c(cur, fid)),
          server   = TRUE
        )
      })
      
      rv$force_select_fid <- fid
      showNotification(sprintf("Added feature: %s", nm), type = "message")
    }) |>bindEvent(input$add_feature, ignoreInit = TRUE)
    
    
    core_cols <- reactive({
      core_table_colnames() %||% character(0)
    })
    
    feature_out_cols <- reactive({
      if (!length(rv$order)) return(character(0))
      unlist(lapply(rv$order, function(fid) rv$features[[fid]]$out_cols))
    })
    
    covariate_cols <- reactive({
      req(im$posterior, im$data_cls)   # or whichever signals covariates are available
      setdiff(names(data.table::as.data.table(im$posterior)), core_cols() %||% character(0))
    })
    
    # Order choices list to be sorted by: base, features, covariates
    ordered_choices_named <- reactive({
      base <- core_cols()
      covs <- covariate_cols()
      # Use display names for columns
      base_choices <- pretty_base_choices(base)
      cov_choices  <- pretty_base_choices(covs)
      
      # Use feature display name
      feature_choices <- if (length(rv$order)) {
        labs <- vapply(rv$order, function(fid) rv$features[[fid]]$label %||% fid, character(1))
        stats::setNames(rv$order, labs)
      } else stats::setNames(character(0), character(0))
      
      c(base_choices, feature_choices, cov_choices)
    })
    
    
    
    # update the single selectize input when relevant pieces change
    observe({
      choices <- ordered_choices_named()
      cur_sel <- input$display_cols %||% character(0)
      vals <- unname(choices)  # valid underlying values (colnames + feature IDs)
      default_sel <- intersect(core_cols(), vals)
      
      if (length(cur_sel) == 0) {
        selected <- default_sel
      } else {
        selected <- intersect(cur_sel, vals)
        if (length(selected) == 0 && length(default_sel) > 0) selected <- default_sel
      }
      
      if (!is.null(rv$force_select_fid) && rv$force_select_fid %in% unname(choices)) {
        selected <- unique(c(selected, rv$force_select_fid))
        rv$force_select_fid <- NULL
      }
      
      updateSelectizeInput(
        session,
        "display_cols",
        choices  = choices,
        selected = selected,
        server   = TRUE
      )
    }) |>bindEvent(list(rv$order, im$posterior, im$data_cls, core_cols(), feature_out_cols(), covariate_cols()), ignoreNULL = FALSE, ignoreInit = FALSE)
    
    
    # Posterior
    get_mean_dt <- function(use_count_scale = FALSE) {
      req(im$model, im$data_cls)
      
      dcls <- im$data_cls
      reg_col   <- dcls$region_column
      date_col  <- dcls$date_column
      denom_col <- dcls$denominator_column
      
      mu <- im$model$summary.fitted.values[, "mean"]
      if (is.null(mu)) return(NULL)
      
      dt <- data.table::as.data.table(dcls$data)
    
      keep <- intersect(unique(c(reg_col, date_col, denom_col)), names(dt))
      if (length(keep) == 0) return(data.table::data.table())
      
      dt <- dt[, ..keep]
      dt[, mu := as.numeric(mu)]
      
      model_scale <- get_model_scale(im$model)
      if (!is.null(denom_col) && denom_col %in% names(dt)) {
        if (model_scale == "count" && !use_count_scale) dt[, mu := mu / get(denom_col)]
        if (model_scale == "prop"  &&  use_count_scale) dt[, mu := mu * get(denom_col)]
      }
      
      if (reg_col %in% names(dt))  dt[, (reg_col) := as.character(get(reg_col))]
      
      out_col <- if (use_count_scale) "mean_count" else "mean_prop"
      dt <- dt[, .(value = mu), by = c(reg_col, date_col)]
      data.table::setnames(dt, "value", out_col)
      dt[]
    }
    
    qdf_props <- reactive({
      req(rv$refresh > 0)
      qdf <- get_posterior_quantiles(im$model, im$data_cls, probs = all_probs(), use_count_scale = FALSE)
      qdf <- normalize_qdf_names(qdf)
      data.table::setDT(qdf)
      qdf
    })
    
    qdf_counts <- reactive({
      req(rv$refresh > 0)
      qdf <- get_posterior_quantiles(im$model, im$data_cls, probs = all_probs(), use_count_scale = TRUE)
      qdf <- normalize_qdf_names(qdf)
      
      dcls <- im$data_cls
      reg_col <- dcls$region_column
      date_col <- dcls$date_column
      data.table::setDT(qdf)
      q_only <- setdiff(names(qdf), c(reg_col, date_col))
      if (length(q_only)) data.table::setnames(qdf, q_only, paste0(q_only, "_count"))
      qdf
    })
    
    # Add single feature and append it to table
    apply_feature <- function(out, f, dcls) {
      ft <- f$feature_type
      sc <- f$feature_scale
      label <- f$label
      
      for (cc in f$out_cols) if (!cc %in% names(out)) out[, (cc) := NA_real_]
      
      if (ft == "mean") {
        use_count <- (sc == "counts")
        key <- cache_key("mean", use_count)
        res <- cache_get(key)
        if (is.null(res)) {
          res <- get_mean_dt(use_count)
          cache_set(key, res)
        }
        mcol <- if (use_count) "mean_count" else "mean_prop"
        out2 <- merge_by_region_date(out, res, dcls)
        out2[, (label) := as.numeric(get(mcol))]
        out2[, (mcol) := NULL]
        return(out2[])
      }
      
      if (ft == "quantile") {
        qn <- fmt_qname(f$params$q %||% 0.5)
        if (sc == "counts") { qdf <- qdf_counts(); src <- paste0(qn, "_count") }
        else { qdf <- qdf_props(); src <- qn }
        
        qdf_one <- slice_qdf(qdf, dcls, src)
        out2 <- merge_by_region_date(out, qdf_one, dcls)
        out2[, (label) := as.numeric(get(src))]
        out2[, (src) := NULL]
        return(out2[])
      }
      
      if (ft == "confidence_interval") {
        ci <- f$params$ci %||% 0.90
        a <- (1 - ci) / 2
        qL <- fmt_qname(a)
        qU <- fmt_qname(1 - a)
        cL <- paste0(label, " Lower")
        cU <- paste0(label, " Upper")
        
        if (sc == "counts") {
          qdf <- qdf_counts()
          srcL <- paste0(qL, "_count")
          srcU <- paste0(qU, "_count")
        } else {
          qdf <- qdf_props()
          srcL <- qL
          srcU <- qU
        }
        
        qdf_two <- slice_qdf(qdf, dcls, c(srcL, srcU))
        out2 <- merge_by_region_date(out, qdf_two, dcls)
        out2[, (cL) := as.numeric(get(srcL))]
        out2[, (cU) := as.numeric(get(srcU))]
        out2[, c(srcL, srcU) := NULL]
        return(out2[])
      }
      
      if (ft == "exceedance_probability") {
        thr <- as.numeric(f$params$threshold %||% 0)
        use_count <- (sc == "counts")
        key <- cache_key("exceed", thr, use_count)
        res <- cache_get(key)
        if (is.null(res)) {
          res <- get_exceedance_probs(
            inla_model = im$model,
            data_cls = im$data_cls,
            threshold = thr,
            use_suffix = TRUE,
            use_count_scale = use_count
          )
          cache_set(key, res)
        }
        out2 <- merge_by_region_date(out, res, dcls)
        ex_col <- grep("^exceedance_prob", names(out2), value = TRUE)[1]
        out2[, (label) := as.numeric(get(ex_col))]
        out2[, (ex_col) := NULL]
        return(out2[])
      }
      out
    }
  
    observe({
      ch <- feature_choice_map()
      if (!length(ch)) {
        showNotification("No features to delete yet.", type = "warning")
        return()
      }
      
      showModal(modalDialog(
        title = "Delete a feature",
        selectInput(
          inputId  = ns("delete_feature_pick"),
          label    = "Select feature to delete",
          choices  = ch,
          selected = unname(ch[[length(ch)]])
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete_prompt"), "Delete…", class = "btn btn-danger")
        ),
        easyClose = TRUE
      ))
    }) |>bindEvent(input$delete_feature, ignoreInit = TRUE)
    
    observe({
      fid <- input$delete_feature_pick
      if (is.null(fid) || !nzchar(fid)) return()
      
      # if user opened the modal and the feature list changed underneath them
      if (is.null(rv$features[[fid]])) {
        showNotification("That feature no longer exists.", type = "warning")
        removeModal()
        return()
      }
      
      removeModal()
      
      lbl <- rv$features[[fid]]$label %||% fid
      
      showModal(modalDialog(
        title = "Confirm deletion",
        tags$div(
          class = "alert alert-warning",
          tags$strong("This action cannot be undone."),
          tags$div(sprintf('You are about to delete: "%s".', lbl))
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete_ok"), "Yes, delete", class = "btn btn-danger")
        ),
        easyClose = FALSE
      ))
    }) |>bindEvent(input$confirm_delete_prompt, ignoreInit = TRUE)
    
    observe({
      fid <- input$delete_feature_pick
      removeModal()
      
      if (is.null(fid) || !nzchar(fid) || is.null(rv$features[[fid]])) {
        showNotification("Feature not found.", type = "error")
        return()
      }
      
      lbl <- rv$features[[fid]]$label %||% fid
      rv$features[[fid]] <- NULL
      rv$order <- setdiff(rv$order, fid)
      if (identical(rv$last_id, fid)) rv$last_id <- tail(rv$order, 1L)
      
      cur_sel <- isolate(input$display_cols %||% character(0))
      new_sel <- setdiff(cur_sel, fid)
      
      updateSelectizeInput(
        session,
        "display_cols",
        choices  = ordered_choices_named(),
        selected = new_sel,
        server   = TRUE
      )
      
      rv$refresh <- rv$refresh + 1L
      showNotification(sprintf("Deleted feature: %s", lbl), type = "message")
    }) |>bindEvent(input$confirm_delete_ok, ignoreInit = TRUE)
    

    
    
    front_cols <- reactive({
      d <- im$data_cls
      c("region", d$region_column, d$date_column, d$numerator_column, d$denominator_column)
    })
    
    core_table_colnames <- reactive({
      req(im$posterior, im$data_cls)
      out <- data.table::as.data.table(im$posterior)
      front0 <- intersect(front_cols(), names(out))
      front0
    })
    
    covariate_colnames <- reactive({
      req(im$posterior, im$data_cls)
      out <- data.table::as.data.table(im$posterior)
      setdiff(names(out), core_table_colnames() %||% character(0))
    })
    
    
    observe({
      core <- core_table_colnames() %||% character(0)
      choices <- pretty_base_choices(core)
      updateSelectizeInput(
        session,
        "base_cols",
        choices  = choices,
        selected = core,
        server   = TRUE
      )
    }) |>bindEvent(list(im$posterior, im$data_cls), ignoreInit = TRUE)

    observe({
      covs <- covariate_colnames() %||% character(0)
      choices <- pretty_base_choices(covs)
      
      cur_sel <- input$cov_cols %||% character(0)
      
      updateSelectizeInput(
        session,
        "cov_cols",
        choices  = choices,
        selected = intersect(cur_sel, covs),
        server   = TRUE
      )
    }) |>bindEvent(list(im$posterior, im$data_cls), ignoreInit = TRUE)
    
    
    # Posterior table
    posterior_tbl <- reactive({
      req(im$posterior, im$data_cls)
      dcls <- im$data_cls
      
      out <- data.table::as.data.table(im$posterior)
      # Selected features (feature IDs)
      sel <- input$display_cols %||% character(0)
      feat_ids_selected <- intersect(sel, rv$order)
      
      core <- core_table_colnames() %||% character(0)
      base_selected <- if (length(sel) == 0) core else intersect(sel, core)
      base_keep <- intersect(base_selected, names(out))
      
      cov_selected <- setdiff(sel, c(core, rv$order))
      cov_keep <- intersect(cov_selected, names(out))
      
      cols_to_keep <- unique(c(base_keep, cov_keep))
      cols_to_keep <- intersect(cols_to_keep, names(out))
      out <- out[, cols_to_keep, with = FALSE]
      
      if (length(sel) == 0) {
        feat_ids_to_apply <- rv$last_id %||% character(0)
      } else {
        feat_ids_to_apply <- intersect(sel, rv$order)
      }

      for (fid in feat_ids_to_apply) {
        f <- rv$features[[fid]]
        if (!is.null(f)) {
          out <- apply_feature(out, f, dcls)
        }
      }
      
      # Keep base columns first
      base_front <- intersect(base_keep, names(out))
      out[, c(base_front, setdiff(names(out), base_front)), with = FALSE][]
    })
    
    table_id <- session$ns("posterior_data")
    
    output$posterior_data <- reactable::renderReactable({
      req(input$dt_digits)
      df <- posterior_tbl(); req(df)
      
      front <- intersect(front_cols(), names(df))
      df <- df[, c(front, setdiff(names(df), front)), with = FALSE]
      
      # display names
      display_names <- map_table_names_to_display(names(df), quantile_suffix = NULL, keep_names = TRUE)
      if (is.null(names(display_names))) names(display_names) <- names(df)
      
      # Get the columns to round
      digits <- max(0, min(10, as.integer(input$dt_digits %||% 2)))
      date_cols <- names(df)[sapply(df, inherits, "Date")]
      
      # only round feature columns
      front <- intersect(front_cols(), names(df))
      feature_cols <- setdiff(names(df), front)
      cols_to_round <- feature_cols[vapply(df[, ..feature_cols], is.numeric, logical(1))]
      
      # column defs
      col_defs <- lapply(names(df), function(col) {
        label <- display_names[[col]] %||% col
        # get column types 
        is_num <- is.numeric(df[[col]])
        is_date <- col %in% date_cols
        is_rounded <- col %in% cols_to_round
        # define column filters
        if (is_num) {
          reactable::colDef(
            name = label,
            align        = "right",
            filterable   = TRUE,
            filterMethod = if (exists("numeric_range_filter_method", envir = globalenv())) numeric_range_filter_method else NULL,
            filterInput  = if (exists("numeric_range_filter_input", envir = globalenv()))
              function(values, name) numeric_range_filter_input(values, name, table_id) else NULL,
            format       = if (is_rounded) reactable::colFormat(digits = digits) else NULL
          )
        } else if (is_date) {
          reactable::colDef(
            name         = label,
            filterable   = TRUE,
            filterMethod = if (exists("date_filter_method", envir = globalenv())) date_filter_method else NULL,
            filterInput  = if (exists("date_filter_input", envir = globalenv()))
              function(values, name) date_filter_input(values, name, table_id) else NULL
          )
        } else {
          reactable::colDef(
            name         = label,
            filterable   = TRUE,
            filterMethod = if (exists("checkbox_filter_method", envir = globalenv())) checkbox_filter_method else NULL,
            filterInput  = if (exists("checkbox_filter_input", envir = globalenv()))
              function(values, name) checkbox_filter_input(values, name, table_id) else NULL
          )
        }
      })
      names(col_defs) <- names(df)
      
      page_size <- min(nrow(df), 10L)
      
      reactable::reactable(
        df,
        columns = col_defs,
        defaultPageSize = page_size,
        pageSizeOptions = c(5, 10, 15, 25, 50, 100),
        searchable = TRUE,
        filterable = TRUE,
        highlight  = TRUE,
        striped    = TRUE,
        bordered   = TRUE,
        resizable  = TRUE,
        wrap       = TRUE,
        defaultColDef = reactable::colDef(
          minWidth = 120,
          headerStyle = list(
            whiteSpace = "normal",
            wordBreak  = "break-word",
            lineHeight = "1.1"
          ),
          style = list(whiteSpace = "nowrap")
        ),
        fullWidth = TRUE,
        theme = BS_REACTABLE_THEME
      )
    })

    outputOptions(output, "posterior_data", suspendWhenHidden = TRUE)
    
    output$download_posterior_csv <- downloadHandler(
      filename = function() paste0("posterior_table_", Sys.Date(), ".csv"),
      content = function(file) {
        df <- posterior_tbl(); req(df)
        front <- intersect(front_cols(), names(df))
        df <- df[, c(front, setdiff(names(df), front)), with = FALSE]
        data.table::fwrite(df, file)
      }
    )
  })
}

# Format quantile columns
fmt_qname <- function(q, digits = 3) {
  q <- suppressWarnings(as.numeric(q))
  out <- prettyNum(round(q, digits), digits = 12, drop0trailing = TRUE)
  sub("^\\.", "0.", out)
}

normalize_qdf_names <- function(qdf) {
  data.table::setDT(qdf)
  old <- names(qdf)
  new <- sub("^(props_|counts?_)", "", old)
  num_like <- !is.na(suppressWarnings(as.numeric(new)))
  if (any(num_like)) new[num_like] <- fmt_qname(new[num_like], digits = 12)
  data.table::setnames(qdf, old, new, skip_absent = TRUE)
  qdf
}

slice_qdf <- function(qdf, data_cls, cols_keep) {
  # isolate relevant columns of returned qdf to merge
  data.table::setDT(qdf)
  reg_col  <- data_cls$region_column
  date_col <- data_cls$date_column
  if (reg_col %in% names(qdf))  qdf[, (reg_col) := as.character(get(reg_col))]
  keep <- intersect(c(reg_col, date_col, cols_keep), names(qdf))
  if (length(keep) == 0) return(data.table::data.table())
  qdf[, ..keep]
}

pretty_base_choices <- function(cols) {
  if (length(cols) == 0) return(setNames(character(0), character(0)))
  pretty_map <- map_table_names_to_display(cols, quantile_suffix = NULL, keep_names = TRUE)
  labels <- cols
  if (!is.null(names(pretty_map))) {
    labels <- unname(pretty_map[cols])
    labels[is.na(labels) | labels == ""] <- cols[is.na(labels) | labels == ""]
  }
  stats::setNames(cols, labels)
}


# Merge dateframes on date and region columns
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

merge_by_region_date <- function(x, y, data_cls) {
  data.table::setDT(x); data.table::setDT(y)
  reg_col <- data_cls$region_column
  date_col <- data_cls$date_column
  
  x[, (reg_col) := as.character(get(reg_col))]
  y[, (reg_col) := as.character(get(reg_col))]
  
  overlap <- setdiff(intersect(names(x), names(y)), c(reg_col, date_col))
  if (length(overlap)) x[, (overlap) := NULL]
  
  data.table::setkeyv(x, c(reg_col, date_col))
  data.table::setkeyv(y, c(reg_col, date_col))
  y[x]
}

# format decimals
fmt_num <- function(x, digits = 6) {
  trimws(formatC(x, format = "fg", digits = digits))
}
