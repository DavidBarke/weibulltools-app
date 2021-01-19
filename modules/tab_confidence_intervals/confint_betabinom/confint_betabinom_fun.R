confint_betabinom_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    title = "Beta Binomial Confidence Bounds",
    r_function_ui(
      id = ns("function")
    )
  )
}

confint_betabinom_fun_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      r_function_server(
        id = "function",
        .values = .values,
        name = "confint_betabinom",
        r_function_arg(
          "x"
        ),
        r_b_lives_arg(
          inputId = ns("b_lives")
        ),
        r_bounds_arg(
          inputId = ns("bounds")
        ),
        r_conf_level_arg(
          inputId = ns("conf_level")
        ),
        r_direction_arg(
          inputId = ns("direction")
        )
      )
    }
  )
}
