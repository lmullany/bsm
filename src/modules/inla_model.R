inla_model_ui <- function(id) {
  btn_class <- "btn-primary btn-sm"
  ns <- NS(id)
  
  nav_panel(
    title = "INLA Model",
    layout_sidebar(
      sidebar = sidebar(
        id = ns("config_sidebar"),
        width = SIDEBAR_WIDTH,
        card("place_holder for inla model")
      ),
      card("place_holder for inla model")
    )
  )
  
}

inla_model_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}
