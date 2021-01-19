confint_betabinom_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      confint_betabinom_fun_ui(
        id = ns("confint_betabinom_fun")
      )
    )
  )
}

confint_betabinom_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      confint_betabinom_fun_server(
        id = "confint_betabinom_fun"
      )
    }
  )
}
