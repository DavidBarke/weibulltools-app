confidence_intervals_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(

  )
}

confidence_intervals_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
    }
  )
}
