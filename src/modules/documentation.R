# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

documentation_ui <- function(id) {
  nav_panel("Documentation", uiOutput(outputId = NS(id)("app_documentation")))
}

documentation_server <- function(id) {
  moduleServer(id,function(input, output, session) {
      output$app_documentation <- renderUI(HTML(markdown::markdownToHTML(
            file="src/documentation/documentation.md",
            fragment.only = TRUE
      )))
    })
}
