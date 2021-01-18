rank_regression_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(

  )
}

rank_regression_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
    }
  )
}
