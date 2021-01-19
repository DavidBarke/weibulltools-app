ml_estimation_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    title = "ML Estimation",
    r_function(
      id = ns("function"),
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
  )
}

ml_estimation_fun_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
    }
  )
}
