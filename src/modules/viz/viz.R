# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

viz_ui <- function(id) {

  ns <- NS(id)
  
  nav_panel(
    title = "Visualization",
    value = ns("viz_main"),
    navset_bar(
      viz_regional_map_ui(ns("viz_region")),
      viz_time_series_ui(ns("viz_time_series")),
      viz_posterior_ui(ns("viz_posterior")),
      navbar_options = list(class = "bg-primary", theme = "dark", underline=FALSE)
    )
  )
  
}

viz_server <- function(id, dc, im, results) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns = session$ns
      
      viz_regional_map_server(id = "viz_region", dc, im, results)
      viz_time_series_server(id = "viz_time_series", im, results)
      viz_posterior_server(id = "viz_posterior", im, results)
      
    
    }
  )
}
