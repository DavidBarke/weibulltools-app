confint_fisher_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(

  )
}

confint_fisher_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
    }
  )
}
