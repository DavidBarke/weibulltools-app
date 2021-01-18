ml_estimation_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(

  )
}

ml_estimation_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
    }
  )
}
