probability_estimation_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      estimate_cdf_ui(
        id = ns("estimate_cdf")
      )
    )
  )
}

probability_estimation_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      estimate_cdf_server(
        id = "estimate_cdf",
        .values = .values
      )
    }
  )
}
