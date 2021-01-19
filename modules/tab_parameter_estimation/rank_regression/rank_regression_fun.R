rank_regression_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    title = "Rank Regression",
    r_function_ui(
      id = ns("function")
    )
  )
}

rank_regression_fun_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      r_function_server(
        id = "function",
        .values = .values,
        name = "rank_regression",
        r_function_arg(
          "x"
        ),
        r_distribution_arg(
          inputId = ns("distribution")
        ),
        r_conf_level_arg(
          inputId = ns("conf_level")
        )
      )

      ns <- session$ns
    }
  )
}
