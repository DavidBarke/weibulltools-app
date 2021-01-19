rank_regression_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      rank_regression_fun_ui(
        id = ns("rank_regression_fun")
      )
    )
  )
}

rank_regression_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      rank_regression_fun_server(
        id = "rank_regression_fun",
        .values = .values
      )
    }
  )
}
