probability_estimation_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(

  )
}

probability_estimation_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
    }
  )
}
