confint_fisher_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    title = "Fisher's Confidence Bounds",
    r_function(
      id = ns("function"),
      name = "confint_fisher",
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
  )
}

confint_fisher_fun_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
    }
  )
}
