confint_fisher_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      confint_fisher_fun_ui(
        id = ns("confint_fisher_fun")
      )
    )
  )
}

confint_fisher_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      confint_fisher_fun_server(
        id = "confint_fisher_fun"
      )
    }
  )
}
