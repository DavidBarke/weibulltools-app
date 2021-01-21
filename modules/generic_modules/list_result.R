list_result_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::uiOutput(
    outputId = ns("accordion")
  )
}

list_result_server <- function(id, .values, obj_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      accordion_items_r <- shiny::reactive({
        purrr::map(names(obj_r()), function(name) {
          output_name <- "item" %_% name

          if (!output_name %in% names(output)) {
            output[[output_name]] <- shiny::renderPrint({
              obj_r()[[name]]
            })
          }

          bs4Dash::accordionItem(
            title = name,
            shiny::verbatimTextOutput(
              outputId = ns("item" %_% name)
            )
          )
        })
      })

      output$accordion <- shiny::renderUI({
        do.call(
          bs4Dash::accordion,
          c(
            list(id = ns("accordion")),
            accordion_items_r()
          )
        )
      })
    }
  )
}
