estimate_cdf_result_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    DT::dataTableOutput(
      outputId = ns("data")
    )
  )
}

estimate_cdf_result_server <- function(id, .values, estimate_cdf_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$data <- DT::renderDataTable({
        estimate_cdf_r()
      })
    }
  )
}
