# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

feature_sidepanel_ui <- function(id, title = "Filters", allow_multiple = TRUE) {
  ns <- NS(id)
  
  heading <- if (allow_multiple) {
    h5("Selected feature(s)")
  } else {
    h5("Selected feature")
  }
  
  tagList(
    tags$style(HTML(sprintf("
      #%s select[multiple],
      #%s select[multiple] {
        height: 15vh !important;
        overflow-y: auto;
      }
    ",
                            ns("selected_list"),
                            ns("add_list")
    ))),
    
    div(
      id = ns("wrapper"),
      heading,
      
      selectInput(
        ns("selected_list"),
        label = NULL,
        choices = character(0),
        selected = character(0),
        multiple = TRUE,
        selectize = FALSE,
        size = if (allow_multiple) NULL else 1,
        width = "100%"
      ),
      tags$small(class = "text-muted", "Click an item to remove it from the selected group."),
      actionButton(
        ns("clear_selected"),
        "Clear selected",
        class = "btn-primary btn-sm",
        style = "width:100%;"
      ),
      tags$hr(),
      h5("Available feature(s)"),
      selectInput(
        ns("add_list"),
        label = NULL,
        choices = character(0),
        selected = character(0),
        multiple = TRUE,
        selectize = FALSE,
        width = "100%"
      ),
      tags$small(class = "text-muted", "Click an item to add it to the selected group."),
      tags$hr(),
      h4(title, style = "margin:0;"),
      
      tags$div(
        style = "margin-top: 8px; margin-bottom: 100px; display: block;",
        
        checkboxGroupInput(
          ns("filter_scale"),
          "Scale",
          choices = c(
            "Counts" = "counts",
            "Proportion" = "proportion",
            "Other" = "other"
          ),
          selected = character(0)
        ),
        
        checkboxGroupInput(
          ns("filter_type"),
          "Feature type",
          choices = c(
            "Mean" = "mean",
            "Posterior Quantile" = "quantile",
            "Confidence Interval" = "confidence_interval",
            "Exceedance Probability" = "exceedance_probability",
            "Change Probability" = "change_probability",
            "Covariate" = "covariate",
            "ID" = "id",
            "Other" = "other"
          ),
          selected = character(0)
        ),
        
        actionButton(
          ns("reset_filters"),
          "Reset filters",
          class = "btn-primary btn-sm",
          style = "width:100%;"
        )
      )
    )
  )
}


feature_sidepanel_server <- function(id,
                                     feature_store,
                                     allow_multiple = TRUE,
                                     reset_clears_selected = FALSE,
                                     initial_selected_id = NULL) {
  moduleServer(id, function(input, output, session) {
    
    get_store <- function() {
      if (is.function(feature_store)) feature_store() else feature_store
    }
    
    df_all <- reactive({
      get_store()$features_df()
    })
    
    normalize_df <- function(df) {
      if (is.null(df) || nrow(df) == 0) return(df)
      
      df$feature_scale[is.na(df$feature_scale) | df$feature_scale == ""] <- "other"
      df$feature_type[is.na(df$feature_type) | df$feature_type == ""] <- "other"
      df$scale_filter <- ifelse(
        df$feature_scale %in% c("counts", "proportion"),
        df$feature_scale,
        "other"
      )
      df$.key <- paste0(df$id, "::", df$feature_scale, "::", df$feature_type)
      df
    }
    
    selected_rv <- reactiveVal(character(0))
    
    observe({
      df <- normalize_df(df_all())
      init_id <- initial_selected_id %||% ""
      cur <- selected_rv() %||% character(0)
      if (!nzchar(init_id) || length(cur) > 0 || is.null(df) || !nrow(df)) return()
      
      match_idx <- which(df$id == init_id)
      if (!length(match_idx)) return()
      selected_rv(df$.key[[match_idx[[1]]]])
    }) %>% bindEvent(df_all(), ignoreInit = FALSE)
    
    make_named_choices <- function(df) {
      if (is.null(df) || nrow(df) == 0) return(setNames(character(0), character(0)))
      vals <- as.character(df$.key)
      labs <- as.character(df$label)
      stats::setNames(vals, labs)
    }
    
    selected_choices_named <- reactive({
      df  <- normalize_df(df_all())
      sel <- selected_rv()
      if (is.null(df) || !nrow(df) || !length(sel)) return(setNames(character(0), character(0)))
      
      df_sel <- df[df$.key %in% sel, , drop = FALSE]
      df_sel <- df_sel[match(sel, df_sel$.key), , drop = FALSE]
      make_named_choices(df_sel)
    })
    
    # Filters only modify the "can-be-added" list (ie filters won't unselect values)
    filtered_features_df <- reactive({
      df <- normalize_df(df_all())
      if (is.null(df) || nrow(df) == 0) return(df)
      
      scales <- input$filter_scale %||% character(0)
      types  <- input$filter_type %||% character(0)
      
      if (length(scales) > 0) df <- df[df$scale_filter %in% scales, , drop = FALSE]
      if (length(types)  > 0) df <- df[df$feature_type  %in% types,  , drop = FALSE]
      
      df
    })
    
    reorder_default_df <- function(df) {
      if (is.null(df) || nrow(df) == 0) return(df)
      df[order(tolower(df$label), df$label, df$id), , drop = FALSE]
    }
    
    add_choices_named <- reactive({
      df  <- normalize_df(filtered_features_df())
      sel <- selected_rv()
      if (is.null(df) || !nrow(df)) return(setNames(character(0), character(0)))
      
      df <- reorder_default_df(df)
      if (length(sel)) df <- df[!(df$.key %in% sel), , drop = FALSE]
      
      make_named_choices(df)
    })
    
    # Move items between selected and can-be-selected lists
    observe({
      updateSelectInput(session, "add_list",
                        choices  = add_choices_named(),
                        selected = character(0))
      
      updateSelectInput(session, "selected_list",
                        choices  = selected_choices_named(),
                        selected = character(0))
    }) %>% bindEvent(add_choices_named(), selected_choices_named(), ignoreInit = FALSE)
    
    observe({
      keys <- input$add_list %||% character(0)
      if (!length(keys)) return()
      
      cur <- selected_rv()
      if (!allow_multiple) {
        selected_rv(keys[[1]])
      } else {
        selected_rv(unique(c(cur, keys)))
      }
      
      updateSelectInput(session, "add_list", selected = character(0))
    }) %>% bindEvent(input$add_list, ignoreInit = TRUE)
    
    # remove selected item from "can-be-selected" list
    observe({
      keys <- input$selected_list %||% character(0)
      if (!length(keys)) return()
      
      cur <- selected_rv()
      selected_rv(setdiff(cur, keys))
      
      updateSelectInput(session, "selected_list", selected = character(0))
    }) %>% bindEvent(input$selected_list, ignoreInit = TRUE)
    
    observe({
      selected_rv(character(0))
    }) %>% bindEvent(input$clear_selected, ignoreInit = TRUE)
    
    observe({
      updateCheckboxGroupInput(session, "filter_scale", selected = character(0))
      updateCheckboxGroupInput(session, "filter_type", selected = character(0))
      
      if (isTRUE(reset_clears_selected)) selected_rv(character(0))
    }) %>% bindEvent(input$reset_filters, ignoreInit = TRUE)
    
    reactive({
      list(
        selected_features = selected_rv(),
        filter_scale      = input$filter_scale %||% character(0),
        filter_type       = input$filter_type %||% character(0),
        add_menu_choices  = add_choices_named()
      )
    })
  })
}
