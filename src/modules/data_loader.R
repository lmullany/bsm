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
    choices=c("Zip Code" = "zip", "County" = "county"),
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
    start = end - 365,
    end = end
  )
  
  # time resolution
  time_res <- selectInput(
    ns("time_res"),
    label = "Time Resolution",
    choices = c("Daily" = "daily", "Weekly" = "weekly", "Monthly" = "monthly"),
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
        width = SIDEBAR_WIDTH,
        geo, 
        states,
        drange,
        time_res,
        synd_panel,
        input_task_button(ns("load_data_btn"), "Load Data")
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
          downloadButton(
            ns("download_data"),
            label="Download Data", 
            class="btn-primary"
          )
        )
      )
    )
  )

}

data_loader_server <- function(id, dc, results, profile) {
  moduleServer(
    id,
    function(input, output, session) {
      
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
      
      data <- reactive({
        
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
          data$data$date<-sapply(data$data$date,week_to_end_date)
        } else if (input$time_res=="daily"){
          data$data$date<-as.Date(data$data$date)
        }
        return(data)
        
      }) |> bindEvent(input$load_data_btn)
      
      output$ingested_data <- renderDT(
        data()$data
      )
      
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
      label = "Target Outcome",
      choices = c(
        "CCDD" = "ccdd",
        "Syndrome" = "synd",
        "Sub-Syndrome" = "subsynd"
      )
    ),
    selectInput(
      inputId = ns("synd_drop_menu"),
      label="Select Type",
      choices = cats
    )
  )
}

