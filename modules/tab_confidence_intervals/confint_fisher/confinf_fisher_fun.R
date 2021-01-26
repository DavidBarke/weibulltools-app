confint_fisher_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "confint_fisher",
    varname = ref_dropdown_ui(
      id = ns("ref_dropdown"),
      varname = "conf_fisher",
      references = "plot_conf"
    ),
    r_function_arg(
      "x",
      shiny::uiOutput(
        outputId = ns("x")
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

      ref_dropdown_server(
        id = "ref_dropdown",
        .values = .values,
        tabNames = c(plot_conf = "confint_fisher")
      )

      output$x <- shiny::renderUI({
        varname_link_ui(
          id = ns("varname_link_ml_estimation"),
          name = attr(ml_estimation_r, "shinymetaVarname", exact = TRUE)
        )
      })

      varname_link_server(
        id = "varname_link_ml_estimation",
        .values = .values,
        tabName = "ml_estimation"
      )

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
          bounds = ..(input$bounds %||% "two_sided"),
          conf_level = ..(input$conf_level %||% 0.95),
          direction = ..(input$direction %||% "y")
        )
      }, varname = "conf_fisher")

      return_list <- list(
        confint_fisher_r = confint_fisher_r
      )

      return(return_list)
    }
  )
}
