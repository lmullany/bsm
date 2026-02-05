# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

extras_ui <- function(id) {
  ns <- NS(id)
  
  nav_item(
    bslib::popover(
      tags$button(
        type = "button",
        class = "btn btn-link nav-link p-0",
        bsicons::bs_icon("gear", title = "Extras")
      ),
      
      div(
        class = "d-grid gap-2",
        
        actionButton(
          ns("open_docs"),
          "Documentation",
          icon = bsicons::bs_icon("book"),
          class = "btn-primary btn-sm"
        ),
        
        # Nested tooltip module UI
        tooltip_ui(ns("tooltip")),
        
        # Light/dark toggle row styled like a Bootstrap button
        div(
          class = "btn btn-primary btn-sm settings-toggle-row",
          role = "button",
          div(
            class = "mode-pill-wrap",
            span(class = "mode-side left",
                 span(class = "mode-text", "Light"),
                 span(class = "mode-icon", bsicons::bs_icon("sun-fill"))
            ),
            tags$button(
              type = "button",
              class = "mode-switch",
              onclick = "
                const html = document.documentElement;
                const cur = html.getAttribute('data-bs-theme') || 'dark';
                const next = (cur === 'dark') ? 'light' : 'dark';
                html.setAttribute('data-bs-theme', next);
                return false;
              "
            ),
            span(class = "mode-side right",
                 span(class = "mode-icon", bsicons::bs_icon("moon-stars-fill")),
                 span(class = "mode-text", "Dark")
            )
          )
        )
      ),
      
      # Namespaced popover id handled entirely inside this module
      id = ns("settings_pop"),
      placement = "bottom",
      options = list(close_button = TRUE)
    )
  )
}

extras_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Own nested modules
    tooltip_server("tooltip")
    
    # Own documentation module as well (docs body returned from server)
    docs_body <- documentation_server("documentation")
    
    # Open docs modal + close popover
    observe({
      shiny::showModal(
        shiny::modalDialog(
          title = "",
          div(
            style = "max-height: 70vh; overflow-y: auto; padding-right: 0.5rem;",
            docs_body
          ),
          size = "l",
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        )
      )
      
      bslib::toggle_popover("settings_pop", show = FALSE)
    }) |>
      bindEvent(input$open_docs, ignoreInit = TRUE)
  })
}
