viz_ui <- function(id) {
  btn_class <- "btn-primary btn-sm"
  ns <- NS(id)
  
  nav_panel(
    title = "Visualization",
    layout_sidebar(
      sidebar = sidebar(
        id = ns("config_sidebar"),
        width = SIDEBAR_WIDTH,
        checkboxInput(ns("add_temporal"), "Add Temporal Trend",value = FALSE),
        checkboxInput(ns("add_rolling"), "Add Rolling Average",value = FALSE),
        checkboxInput(ns("add_rescaled"), "Add Rescale",value = FALSE),
        selectizeInput(ns("viz_regions"), "Select Region(s)", choices=NULL, multiple=TRUE)
      ),
      navset_bar(
        nav_panel(
          title = "Region-Wide Map",
          layout_sidebar(
            leafletOutput(ns("region_map")),
            sidebar = sidebar(
              radioButtons(
                inputId = ns("map_metric"),
                label="Metric",
                choices = c(
                  "Mean" = "mean", 
                  "Median" = "median",
                  "Quantile" = "quantile",
                  "Exceedance" = "exceedance"
                ),
                inline = TRUE,
                selected = "mean"
              ),
              conditionalPanel(
                condition = "input.map_metric != 'exceedance'",
                radioButtons(
                  inputId = ns("metric_counts"),
                  label="Counts/Proportions",
                  choices = c("Counts", "Proportion"),
                  selected = "Counts"
                ),
                ns = ns
              ),
              conditionalPanel(
                condition = "input.map_metric == 'quantile'",
                sliderInput(
                  inputId = ns("metric_quantile"),label = "Quantile",min = 0,max=1,step = 0.01,value=0.5
                ),
                ns = ns
              ),
              conditionalPanel(
                condition = "input.map_metric == 'exceedance'",
                numericInput(inputId = ns("metric_exceedance"), label = "Exceedance Threshold", value=10),
                ns = ns
              ),
              position = "right"
            )
              
            
          )

        ),
        nav_panel(
          title = "Plots",
          plotOutput(ns("plots"))
        ),
        nav_panel(
          title="Posterior Data",
          card(
            card_body(DTOutput(ns("posterior_data"))),
            card_footer(downloadButton(ns("download_posterior_btn"), "Download", class = "btn-primary"))
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
      observe({
        updateSelectizeInput(
          inputId = "viz_regions",
          choices = results$data[["region"]] |> unique()
        )
      })
      
      output$posterior_data <- renderDT({
        req(im$posterior)
        im$posterior
      })
      
      map_params <- reactive({
        # create a list from the overall map sidebar
        l <- list(
          metric = input$map_metric,
          use_count = input$metric_counts == "Counts",
          quantile = input$metric_quantile,
          threshold = input$metric_exceedance
        )
        return(l)
      })

      map_data <- reactive({
        get_map_data(
          model = im$model,
          data_cls = im$data_cls,
          params = map_params()
        )
      }) |> bindEvent(map_params())
      
      region_map <- reactive({
        
        target_date = im$data_cls$data[[im$data_cls$date_col]] |> max()
        
        make_map(
          map_data = map_data(),
          target_date = target_date
        )  
      }) |> bindEvent(map_data())
      
      output$region_map <- renderLeaflet(region_map())
      
      # For now, making all the plots, because they are fast
      plots <- reactive({
        req(im$posterior)
        make_timeseries_plots(res_data = im$posterior, date_col = "date", use_prop = TRUE, F, F, F)
      })
      
      # Download posterior
      output$download_posterior_btn <- downloadHandler(
        filename = "posterior_data.csv" , content = \(file) data.table::fwrite(im$posterior, file)
      )
      
      output$plots <- renderPlot({
        req(input$viz_regions)
        req(plots())
        subplots <- plots()[input$viz_regions]
        do.call("grid.arrange", c(subplots, ncol = min(c(2, length(subplots)))))
      })
      
      
      # Now, when we choose the locations, we create the plots
      
      # plots <- reactive({
      #   
      # })
      
    }
  )
}
