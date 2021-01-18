parameter_estimation_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      bs4Dash::tabBox(
        id = ns("parameter_estimation"),
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        type = "pills",
        side = "right",
        title = "Parametric Models",
        shiny::tabPanel(
          title = "ml_estimation",
          ml_estimation_ui(
            id = ns("ml_estimation")
          )
        ),
        shiny::tabPanel(
          title = "rank_regression",
          rank_regression_ui(
            id = ns("rank_regression")
          )
        )
      )
    )
  )
}

parameter_estimation_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      ml_estimation_server(
        id = "ml_estimation",
        .values = .values
      )

      rank_regression_server(
        id = "rank_regression",
        .values = .values
      )
    }
  )
}
