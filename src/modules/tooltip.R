tooltip_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("toggle_tooltips"), "Hide Tooltips")
  )
}


tooltip_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      tooltips_enabled <- reactiveVal(TRUE)
    
      observe({
        
        if (tooltips_enabled()) {
          # Turn OFF tooltips
          shinyjs::runjs("
        var triggers = document.querySelectorAll('[data-bs-toggle=\"tooltip\"]');

        triggers.forEach(function (el) {
          var tooltip = bootstrap.Tooltip.getOrCreateInstance(el);
          tooltip.hide();
          tooltip.disable();
          el.style.display = 'none';   // hide the info icon
        });

        document.querySelectorAll('.tooltip.show').forEach(function (tip) {
          tip.classList.remove('show');
        });
      ");
          
          tooltips_enabled(FALSE)
          
        } else {
          # Turn ON tooltips
          shinyjs::runjs("
        var triggers = document.querySelectorAll('[data-bs-toggle=\"tooltip\"]');

        triggers.forEach(function (el) {
          var tooltip = bootstrap.Tooltip.getOrCreateInstance(el);
          tooltip.enable();
          el.style.display = '';  // unhide the icon (restore default display)
        });
      ");
          
          tooltips_enabled(TRUE)
        }
        
        b_label <- if (tooltips_enabled()) "Hide Tooltips" else "Show Tooltips"
        updateActionButton(
          inputId = "toggle_tooltips",
          label = b_label
        )
        
      }) |> bindEvent(input$toggle_tooltips)
      
    }
  )
}

