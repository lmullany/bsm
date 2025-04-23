data_loader_ui <- function(id) {

  btn_class <- "btn-primary btn-sm"
  ns <- NS(id)
  
  nav_panel(
    title = "Data Loader",
    layout_sidebar(
      sidebar = sidebar(
        id = ns("config_sidebar"),
        width = SIDEBAR_WIDTH,
        card("place_holder for data loading")
      ),
      card("place_holder for data loading")
    )
  )

}

data_loader_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}
