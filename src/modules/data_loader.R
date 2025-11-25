# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

cat_values = get_categorical_values(profile = CREDENTIALS$profile)

label_list_dl <- list(
  geo_res = list( 
    l = "Geographic Resolution",
    m = "Select geographic resolution for ESSENCE query used to retrieve training data."
  ),
  date_range = list(
    l = "Date Range",
    m = "Select a start and end date (inclusive) for the ESSENCE query. For weekly queries, start dates should fall on a Sunday and end dates should fall on a Saturday to avoid partial weeks."
  ),
  temporal_res = list(
    l = "Time Resolution",
    m = "Select a temporal resolution for the ESSENCE query."
  ),
  target_type = list(
    l = "Select Type",
    m = "Select diagnostic criteria type (CCDD, syndrome or subsyndrome) for filtering records in the ESSENCE query."
  ),
  target_code = list(
    l = "Target Outcome",
    m = "Select the specific diagnostic category or code to use when filtering records in the ESSENCE query.")
)

button_list_dl <-list(
  run_query = "Submit ESSENCE query to load data.",
  load_query = "Load a saved query from file.",
  select_saved = paste0(
    "Open file browser to select a saved query on your local machine. ",
    "Saved queries are zip files containing json and rds objects with the ",
    "file suffix .bsm_query."),
  download_csv = "Download retrieved data and save as a csv file on your local machine.",
  save_query = "Save the query to a bsm_query file so that it can be reloaded later."
)

data_loader_ui <- function(id) {

  ns <- NS(id)
  
  ########################
  # Input Widgets
  ########################
  
  # syndrome selection accordion panel
  synd_panel <- create_syndrome_inputs(ns=ns, cats = cat_values$ccdd_cats)
  
  # geographic resolution: zip or county
  geo = selectInput(
    ns("geo_res"),
    choices=c("County" = "county"), # "Zip Code" = "zip", 
    selected="county",
    label=labeltt(label_list_dl[["geo_res"]])
  )
  
  # state selection - use module - call ui
  states = state_selector_ui(ns("state_selector"))
  
  # date range
  offset = (as.POSIXlt(Sys.Date())$wday -6)%%7
  end = Sys.Date()-offset
  drange = dateRangeInput(
    ns("drange"),
    label=labeltt(label_list_dl[["date_range"]]),
    start = end - 60,
    end = end
  )
  
  # time resolution
  time_res <- selectInput(
    ns("time_res"),
    label=labeltt(label_list_dl[["temporal_res"]]),
    choices = c("Weekly" = "weekly"), #"Daily" = "daily"),"Monthly" = "monthly"),
    selected = "weekly"
  )
  
  
  ########################
  # Nav Panel to Return
  ########################
  nav_panel(
    title = "Data Loader",
    layout_sidebar(
      sidebar = sidebar(
        id = ns("config_sidebar"),
        width = SIDEBAR_WIDTH*2,
        geo, 
        states,
        drange,
        time_res,
        synd_panel,
        layout_columns(
          add_button_hover(title = button_list_dl[["run_query"]],
            input_task_button(ns("load_data_btn"), "Query ESSENCE")),
          add_button_hover(title = button_list_dl[["load_query"]],
            input_task_button(ns("load_saved_query"), "Load Saved Query")),
          width = c(6,6)
        ),
        uiOutput(ns("zipfile_ui"))
      ),
      card(
        card_header("Data", class="bg-primary"),
        card_body(
          withSpinner(
            DTOutput(ns("ingested_data")),
            caption = "Pulling data via API / Please wait",
            color = bs_get_variables(theme=THEME,"primary")
          )
        ),
        card_footer(
          uiOutput(ns("download_ui"))
        )
      )
    )
  )

}



data_loader_server <- function(id, dc, results, profile) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      data <- reactiveVal(NULL)

      # Call the state selector server
      state_selector_server("state_selector", dc)
      
      # Monitor to fill global reactives
      observe(results$data <- data()$data)
      observe(dc$time_res <- input$time_res)
      
      # Update the choices for syndromic categories
      observe({
        req(input$synd_cat)
        # get the set of choices
        sc = list(ccdd=cat_values$ccdd_cats,
                  synd=cat_values$syndromes,
                  subsynd=cat_values$subsyndromes)[[input$synd_cat]]
        
        if(input$synd_cat == "ccdd") selected="CDC COVID-Specific DD v1"
        else selected = NULL
        
        updateSelectInput(
          session = session,
          inputId = "synd_drop_menu",
          choices = sc,
          selected = selected
        )
      })
      
      query_data <- reactive({
        
        # --------------------------
        # Syndromic categories
        # --------------------------
        synd_bits <- list(
          "ccdd" = c("mgs" = "chiefcomplaintsubsyndromes", "cat" = "ccddCategory"),
          "synd" = c("mgs" = "essencesyndromes", "cat" = "medicalGrouping"),
          "subsynd" = c("mgs" = "chiefcomplaintsubsyndromes", "cat" = "medicalGrouping")
        )
        
        med_group_sys = synd_bits[[input$synd_cat]]["mgs"]
        categ_info = list(cat_class = synd_bits[[input$synd_cat]]["cat"],
                          cat_value = xml2::url_escape(tolower(input$synd_drop_menu)))
        
        dput(categ_info)
        
        data<- get_data(
          sd=input$drange[1],
          ed=input$drange[2],
          time_res=input$time_res,
          geo_res=input$geo_res,
          state_filter=dc$states,
          county_filter=dc$selected_counties,
          med_group_sys = med_group_sys,
          categ_info = categ_info, 
          profile = profile()
        )
        
        if (input$time_res=="weekly"){
          #data$data$date<-sapply(data$data$date,week_to_end_date)
          data$data <- wk_to_date(data$data, "date")
        } else if (input$time_res=="daily"){
          data$data$date<-as.Date(data$data$date)
        }
        return(data)
        
      }) |> bindEvent(input$load_data_btn)
      
      output$zipfile_ui <- renderUI({
        req(input$load_saved_query)   # only after button is clicked
        add_button_hover(title = button_list_dl[["select_saved"]], 
            fileInput(ns("zipfile"), "Select Saved Query", 
                      accept = ".bsm_query"))
      })
      
      # set up a reactive to hold user-recalled data (previously saved)
      loaded_data <- reactiveVal(NULL)
      
      
      observe({
        req(input$zipfile)
        validate(need(file.exists(input$zipfile$datapath), "Upload did not complete yet"))
        
        saved_query_info <- load_saved_query_file(input$zipfile$datapath)
        vals <- saved_query_info[["query_values"]]
        
        updateSelectInput(inputId = "time_res", selected = vals$time_res)
        updateDateRangeInput(inputId = "drange", start = vals$drange[1],end = vals$drange[2])
        updateSelectInput(inputId = "geo_res", selected = vals$geo_res)
        updateSelectInput(inputId = "states", selected = vals$states)
        updateSelectInput(inputId = "synd_cat", selected = vals$synd_cat)
        updateSelectInput(inputId = "synd_drop_menu", selected = vals$synd_val)
        
        loaded_data(list(data = saved_query_info[["data"]]))
      }) |> bindEvent(input$zipfile)
      
      
      observe(data(query_data())) |> bindEvent(query_data())
      observe(data(loaded_data())) |> bindEvent(loaded_data())  
      
      output$download_ui <- renderUI({
        req(!is.null(data()))
        tagList(
          layout_column_wrap(
            add_button_hover(title = button_list_dl[["download_csv"]],
                downloadButton(ns("download_data"),
                           "Download to CSV",
                           class = "btn-primary")),
            add_button_hover(title = button_list_dl[["save_query"]], 
                downloadButton(ns("save_query"),
                           "Save Query",
                           class = "btn-primary"))
          )
        )
      })
      
      
      output$save_query <- downloadHandler(
        filename = function() {
          paste0("query-", Sys.Date(), ".bsm_query")
        },
        content = function(file) {
          # describe saved query
          vals <- list(
            time_res    = input$time_res,
            drange = input$drange,
            geo_res = input$geo_res,
            states    = input$states,
            synd_cat = input$synd_cat,
            synd_val = input$synd_drop_menu
          )
          
          json_name <- tempfile(fileext = ".json")
          rds_name  <- tempfile(fileext = ".rds")
          jsonlite::write_json(vals, json_name, pretty = TRUE, auto_unbox = TRUE)
          saveRDS(data()$data, rds_name)
          zip::zipr(file, files = c(rds_name,json_name))
        },
        contentType = "application/zip"
        ) 
      
      
      output$ingested_data <- renderDT({
        req(data()$data)
        # identify columns to round
        cols_to_round <- non_integer_cols_to_round(data()$data)
        
        DT::datatable(
          data()$data,
          colnames = map_table_names_to_display(colnames(data()$data)),
          rownames = F
        ) |> formatRound(cols_to_round, digits=4)
          
      })
      
      output$download_data <- downloadHandler(
        filename = "data.csv" , content = \(file) data.table::fwrite(data()$data, file)
      )
      
    }
  )
}

### data loader module helper functions ###
create_syndrome_inputs <- function(ns, cats) {
  tagList(
    radioButtons(
      inputId = ns("synd_cat"),
      label = labeltt(label_list_dl[["target_type"]]),
      choices = c(
        "Chief Complaint and Discharge Diagnosis Category" = "ccdd",
        "Syndrome" = "synd",
        "Sub-Syndrome" = "subsynd"
      )
    ),
  
    selectInput(
      inputId = ns("synd_drop_menu"),
      label = labeltt(label_list_dl[["target_code"]]),
      choices = cats
    )
  )
}




