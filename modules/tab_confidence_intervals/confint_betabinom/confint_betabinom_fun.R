confint_betabinom_fun_ui <- function(id, conf_name) {
  ns <- shiny::NS(id)

  r_function(
    name = "confint_betabinom",
    varname = conf_name,
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

confint_betabinom_fun_server <- function(id, .values, rank_regression_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      shiny::observeEvent(b_lives_r(), {
        shiny::updateTextInput(
          inputId = "b_lives",
          value = paste(b_lives_r(), collapse = ", ")
        )
      })

      b_lives_r <- shiny::reactive({
        extract_nums(input$b_lives %||% "0.01, 0.1, 0.5")
      }) %>%
        shiny::debounce(1000)

      confint_betabinom_r <- shinymeta::metaReactive({
        confint_betabinom(
          ..(rank_regression_r()),
          b_lives = ..(b_lives_r()),
          bounds = ..(input$bounds %||% "two_sided"),
          conf_level = ..(input$conf_level %||% 0.95),
          direction = ..(input$direction %||% "y")
        )
      }, varname = "conf_bb")

      return_list <- list(
        confint_betabinom_r = confint_betabinom_r
      )

      return(return_list)
    }
  )
}
