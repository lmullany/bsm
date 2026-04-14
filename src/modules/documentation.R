# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

documentation_ui <- function(id) {
  uiOutput(outputId = NS(id)("app_documentation"))
}

register_documentation_resources <- local({
  registered <- FALSE
  
  function() {
    if (registered) return(invisible(NULL))
    
    shiny::addResourcePath(
      prefix = "documentation-screenshots",
      directoryPath = normalizePath("src/documentation/screenshots", winslash = "/", mustWork = TRUE)
    )
    
    shiny::addResourcePath(
      prefix = "documentation-tutorial-screenshots",
      directoryPath = normalizePath("src/documentation/tutorial_screenshots", winslash = "/", mustWork = TRUE)
    )
    
    registered <<- TRUE
    invisible(NULL)
  }
})

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

tutorial_page_ui <- function() {
  register_documentation_resources()
  
  page_fillable(
    theme = THEME,
    title = "BSM Tutorial",
    global_ui_tags,
    tags$style(HTML("
      @media print {
        .tutorial-toolbar {
          display: none !important;
        }

        .tutorial-page img {
          max-width: 100% !important;
          page-break-inside: avoid;
        }

        .tutorial-page figure,
        .tutorial-page h3,
        .tutorial-page h4 {
          page-break-inside: avoid;
        }
      }
    ")),
    div(
      class = "container py-4 tutorial-page",
      style = "max-width: 1100px;",
      div(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-2 mb-3 tutorial-toolbar",
        tags$div(
          tags$h2("Bayesian Spatiotemporal Modeling Tutorial", class = "mb-1"),
          tags$p(
            class = "text-muted mb-0",
            "Use this window alongside the main app so the walkthrough stays visible while you work."
          )
        ),
        div(
          class = "d-flex gap-2 flex-wrap",
          tags$button(
            type = "button",
            class = "btn btn-primary btn-sm",
            onclick = "window.print();",
            "Download PDF"
          ),
          tags$a(
            href = "./",
            class = "btn btn-primary btn-sm",
            "Return to Main App"
          )
        )
      ),
      uiOutput("tutorial_page_content")
    )
  )
}

documentation_server <- function(id) {
  moduleServer(id,function(input, output, session) {
      register_documentation_resources()
      output$app_documentation <- renderUI(
        render_documentation_content("src/documentation/documentation.md")
      )
    }
  )
}
