confint_fisher_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "confint_fisher",
    varname = ref_dropdown(
      varname = r_function_varname("conf_fisher"),
      ref_tbl = tibble::tibble(
        label = "plot_conf",
        reference = "plot_conf",
        tabName = "confint_fisher"
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

confint_fisher_fun_server <- function(id, .values, ml_estimation_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      mle_varname <- attr(ml_estimation_r, "shinymetaVarname", exact = TRUE)

      bounds_r <- shiny::reactive({
        input$bounds %||% "two_sided"
      })

      conf_level_r <- shiny::reactive({
        input$conf_level %||% 0.95
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
          x = mle_varname,
          b_lives = format_vector(b_lives_r()),
          bounds = bounds_r(),
          conf_level = conf_level_r(),
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
          tabName = "ml_estimation",
          varname = mle_varname
        )
      })

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

      confint_fisher_r <- shinymeta::metaReactive({
        confint_fisher(
          ..(ml_estimation_r()),
          b_lives = ..(b_lives_r()),
          bounds = ..(bounds_r()),
          conf_level = ..(conf_level_r()),
          direction = ..(direction_r())
        )
      }, varname = "conf_fisher")

      return_list <- list(
        confint_fisher_r = confint_fisher_r
      )

      return(return_list)
    }
  )
}
