confint_betabinom_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(

  )
}

confint_betabinom_fun_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
    }
  )
}
