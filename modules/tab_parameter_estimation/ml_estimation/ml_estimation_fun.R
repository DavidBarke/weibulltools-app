ml_estimation_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    title = "ML Estimation",
    r_function_ui(
      id = ns("function")
    )
  )
}

ml_estimation_fun_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      r_function_server(
        id = "function",
        .values = .values,
        name = "ml_estimation",
        r_function_arg(
          "x"
        ),
        r_distribution_arg(
          inputId = ns("distribution")
        ),
        r_function_arg(
          "wts",
          "rep(1, nrow(x))"
        ),
        r_conf_level_arg(
          inputId = ns("conf_level")
        )
      )
    }
  )
}
