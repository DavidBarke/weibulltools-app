ml_estimation_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      ml_estimation_fun_ui(
        id = ns("ml_estimation_fun")
      )
    )
  )
}

ml_estimation_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      ml_estimation_fun_server(
        id = "ml_estimation_fun",
        .values = .values
      )
    }
  )
}
