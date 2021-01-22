list_result_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::uiOutput(
    outputId = ns("list")
  )
}

list_result_server <- function(id, .values, obj_r, dynamic = FALSE) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      items_r <- shiny::reactive({
        items <- purrr::map2(names(obj_r()), seq_along(obj_r()), function(name, index) {
          output_name <- "item" %_% index

          if (!output_name %in% names(output)) {
            output[[output_name]] <- shiny::renderPrint({
              obj_r()[[index]]
            })
          }

          bs4Dash::box(
            width = 12,
            title = name,
            shiny::verbatimTextOutput(
              outputId = ns("item" %_% index)
            )
          )
        })

        items
      })

      output$list <- shiny::renderUI({
        items <- if (dynamic) {
          items_r()
        } else {
          shiny::isolate(items_r())
        }

        shiny::fluidRow(items)
      })
    }
  )
}
