# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

documentation_ui <- function(id) {
  uiOutput(outputId = NS(id)("app_documentation"))
}

render_documentation_content <- function(path) {
  if (requireNamespace("markdown", quietly = TRUE)) {
    return(htmltools::HTML(
      markdown::markdownToHTML(file = path, fragment.only = TRUE)
    ))
  }
  
  doc_text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  
  if (requireNamespace("commonmark", quietly = TRUE)) {
    return(htmltools::HTML(
      commonmark::markdown_html(doc_text)
    ))
  }
  
  tags$div(
    class = "alert alert-warning",
    tags$strong("Markdown rendering package not installed."),
    tags$p(
      "Install the ",
      tags$code("markdown"),
      " package for fully formatted documentation. Showing plain text for now."
    ),
    tags$pre(
      style = "white-space: pre-wrap; margin-bottom: 0;",
      doc_text
    )
  )
}

documentation_server <- function(id) {
  moduleServer(id,function(input, output, session) {
      output$app_documentation <- renderUI(
        render_documentation_content("src/documentation/documentation.md")
      )
    }
  )
}
