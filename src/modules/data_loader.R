cat_values = get_categorical_values(profile = CREDENTIALS$profile)

data_loader_ui <- function(id) {

  btn_class <- "btn-primary btn-sm"
  ns <- NS(id)
  
  ########################
  # Input Widgets
  ########################
  
  # syndrome selection accordion panel
  synd_panel <- create_syndrome_inputs(ns=ns, cats = cat_values$ccdd_cats)
  
  # geographic resolution: zip or county
  geo = selectInput(
    ns("geo_res"),
    "Geographic Resolution",
    choices=c("County" = "county"), # "Zip Code" = "zip", 
    selected="county"
  )
  
  # state selection
  states = selectizeInput(
    ns("states"),
    label = "State(s)",
    choices=sort(c("DC",state.abb)),
    multiple=T, # allow multiple,
    selected = DEFAULT_STATES
  )
  
  # date range
  offset = (as.POSIXlt(Sys.Date())$wday -6)%%7
  end = Sys.Date()-offset
  drange = dateRangeInput(
    ns("drange"),
    "Date Range",
    start = end - 60,
    end = end
  )
  
  # time resolution
  time_res <- selectInput(
    ns("time_res"),
    label = "Time Resolution",
    choices = c("Weekly" = "weekly"),#"Daily" = "daily", , "Monthly" = "monthly"),
    selected = "weekly"
  )
  
  
  ########################
  # Nav Panel to Return
  ########################
  geo_res_title = paste0("Select geographic resolution for ESSENCE query used to ",
                     "retrieve training data.")
  state_title = paste0("Select a list of states to include in query. ",
                       "All subdivisions (based on Geographic Resolution) for ", 
                       "the selected states will be added to the query.")
  date_range_title = paste0(
    "Select a start and end date (inclusive) for the ESSENCE query. ",
    "For weekly queries, start dates should fall on a Sunday ",
    "and end dates should fall on a Saturday to avoid partial weeks."
  )
  temporal_res_title = paste0(
    "Select a temporal resolution for the ESSENCE query."
  )
  query_title = "Submit ESSENCE query to load data."
  load_query_title = "Load a saved query from file."
  nav_panel(
    title = "Data Loader",
    layout_sidebar(
      sidebar = sidebar(
        id = ns("config_sidebar"),
        width = SIDEBAR_WIDTH*2,
        div(title =  geo_res_title, geo), 
        div(title = state_title,states),
        div(title = date_range_title,drange),
        div(title = temporal_res_title,time_res),
        synd_panel,
        div(title = query_title, 
            input_task_button(ns("load_data_btn"), "Query ESSENCE")),
        div(title = load_query_title,
            input_task_button(ns("load_saved_query"), "Load Saved Query")),
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
          state_filter=input$states,
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
      
      select_saved_title = paste0(
        "Open file browser to select a saved query on your local machine. ",
        "Saved queries are zip files containing json and rds objects with the ",
        "file suffix .bsm_query.")
                           
      output$zipfile_ui <- renderUI({
        req(input$load_saved_query)   # only after button is clicked
        div(title = select_saved_title, 
            fileInput(ns("zipfile"), "Select Saved Query", 
                      accept = ".bsm_query"))
      })
      
      loaded_data <- reactiveVal(NULL)
      observeEvent(input$zipfile,{
        req(input$zipfile)
        validate(need(file.exists(input$zipfile$datapath), "Upload did not complete yet"))
        tmpdir <- tempfile()   # creates a unique temp folder
        dir.create(tmpdir)
        
        
        unzip(input$zipfile$datapath, exdir = tmpdir)
        files <- list.files(tmpdir, full.names = TRUE)
        rds_file  <- files[grepl("\\.rds$", files, ignore.case = TRUE)]
        json_file <- files[grepl("\\.json$", files, ignore.case = TRUE)]
        validate(
          need(length(rds_file) > 0, "No RDS file found in zip"),
          need(length(json_file) > 0, "No JSON file found in zip")
        )
        # load dataset
        tbl <- readRDS(rds_file[1])
        
        # load JSON only for UI updates
        vals <- jsonlite::read_json(json_file[1], simplifyVector = TRUE)
        updateSelectInput(session, "time_res", selected = vals$time_res)
        updateDateRangeInput(session, "drange",
                             start = vals$drange[1],
                             end   = vals$drange[2])
        updateSelectInput(session, "geo_res", selected = vals$geo_res)
        updateSelectInput(session, "states", selected = vals$states)
        updateSelectInput(session, "synd_cat", selected = vals$synd_cat)
        updateSelectInput(session, "synd_drop_menu", selected = vals$synd_val)
        
        loaded_data(list(data = tbl))
      }) 
      
      
      observeEvent(query_data(), {
        data(query_data())
      })
      
      observeEvent(loaded_data(), {
        data(loaded_data())
      })
      download_csv_title = paste0(
        "Download retrieved data and save as a csv file on your local machine.")
      save_query_title = paste0("Save the query to a bsm_query file so that ", 
                                "it can be reloaded later.")
      output$download_ui <- renderUI({
        req(!is.null(data()))
        tagList(
          div(title = download_csv_title,
              downloadButton(ns("download_data"),
                         "Download to CSV",
                         class = "btn-primary")),
          div(title = save_query_title, 
              downloadButton(ns("save_query"),
                         "Save Query",
                         class = "btn-primary"))
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
        data()$data
      })
      
      output$download_data <- downloadHandler(
        filename = "data.csv" , content = \(file) data.table::fwrite(data()$data, file)
      )
      
    }
  )
}

### data loader module helper functions ###
create_syndrome_inputs <- function(ns, cats) {
  target_type_title = paste0("Select diagnostic criteria type (CCDD, syndrome ",
                             "or subsyndrome) for filtering records in the ",
                             "ESSENCE query.")
  target_code_title = paste0("Select the specific diagnostic category or code ",
                             "to use when filtering records in the ",
                             "ESSENCE query.")
  tagList(
    div(title = target_type_title,
        radioButtons(
      inputId = ns("synd_cat"),
      label = "Target Outcome",
      choices = c(
        "CCDD" = "ccdd",
        "Syndrome" = "synd",
        "Sub-Syndrome" = "subsynd"
      )
    )),
    div(title = target_code_title,
        selectInput(
      inputId = ns("synd_drop_menu"),
      label="Select Type",
      choices = cats
    )
  ))
}




