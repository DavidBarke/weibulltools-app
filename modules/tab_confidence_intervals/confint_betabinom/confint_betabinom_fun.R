confint_betabinom_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "confint_betabinom",
    varname = ref_dropdown(
      varname = r_function_varname("conf_bb"),
      ref_tbl = tibble::tibble(
        label = "plot_conf",
        reference = "plot_conf",
        tabName = "confint_betabinom"
      )
    ),
    placeholder = shiny::uiOutput(
      outputId = ns("placeholder"),
      container = htmltools::pre
    ),
    r_function_arg(
      "x",
      shiny::uiOutput(
        outputId = ns("x"),
        container = htmltools::pre
      )
    ),
    b_lives_ui(
      id = ns("b_lives")
    ),
    r_bounds_arg(
      inputId = ns("bounds")
    ),
    conf_level_ui(
      id = ns("conf_level")
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

      rr_varname <- attr(rank_regression_r, "shinymetaVarname", exact = TRUE)

      bounds_r <- shiny::reactive({
        input$bounds %||% "two_sided"
      })

      direction_r <- shiny::reactive({
        input$direction %||% "y"
      })

      output$placeholder <- shiny::renderUI({
        glue::glue(
          '
          x = {x},
          b_lives = {b_lives},
          bounds = "{bounds}",
          conf_level = {conf_level},
          direction = "{direction}"
          ',
          x = rr_varname,
          b_lives = format_vector(b_lives_return$b_lives_r()),
          bounds = bounds_r(),
          conf_level = conf_level_return$conf_level_r(),
          direction = direction_r()
        )
      })

      shiny::outputOptions(
        output,
        "placeholder",
        suspendWhenHidden = FALSE
      )

      output$x <- shiny::renderUI({
        varname_link(
          tabName = "rank_regression",
          varname = rr_varname
        )
      })

      confint_betabinom_r <- shinymeta::metaReactive({
        confint_betabinom(
          ..(rank_regression_r()),
          b_lives = ..(b_lives_return$b_lives_r()),
          bounds = ..(bounds_r()),
          conf_level = ..(conf_level_return$conf_level_r()),
          direction = ..(direction_r())
        )
      }, varname = "conf_bb")

      conf_level_return <- conf_level_server(
        id = "conf_level",
        .values = .values
      )

      b_lives_return <- b_lives_server(
        id = "b_lives",
        .values = .values
      )

      return_list <- list(
        confint_betabinom_r = confint_betabinom_r
      )

      return(return_list)
    }
  )
}
