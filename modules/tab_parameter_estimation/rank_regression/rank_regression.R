rank_regression_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      bs4Dash::box(
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        title = "Rank Regression",
        rank_regression_fun_ui(
          id = ns("rank_regression_fun")
        ),
        plot_mod_fun_ui(
          id = ns("plot_mod_fun")
        )
      )
    )
  )
}

rank_regression_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      rr_return <- rank_regression_fun_server(
        id = "rank_regression_fun",
        .values = .values
      )

      plot_mod_fun_server(
        id = "plot_mod_fun",
        .values = .values,
        model_r = rr_return$rank_regression_r
      )
    }
  )
}
