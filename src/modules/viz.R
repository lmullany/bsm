viz_ui <- function(id) {
  btn_class <- "btn-primary btn-sm"
  ns <- NS(id)
  
  nav_panel(
    title = "Visualization",
    layout_sidebar(
      sidebar = sidebar(
        id = ns("config_sidebar"),
        width = SIDEBAR_WIDTH,
        card("place_holder for visualization")
      ),
      card("place_holder for visualizatin")
    )
  )
  
}

viz_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}
