reliability_data_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(

  )
}

reliability_data_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
    }
  )
}
