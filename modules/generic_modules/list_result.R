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

          is_table <- is.data.frame(obj_r()[[index]])
          renderFun <- if (is_table) DT::renderDataTable else shiny::renderPrint
          outFun <- if (is_table) DT::dataTableOutput else shiny::verbatimTextOutput

          if (!output_name %in% names(output)) {
            output[[output_name]] <- renderFun({
              obj_r()[[index]]
            })
          }

          bs4Dash::box(
            width = 12,
            collapsed = TRUE,
            title = name,
            outFun(
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
