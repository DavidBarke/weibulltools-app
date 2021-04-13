rank_regression_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "rank_regression",
    varname = ref_dropdown(
      varname = r_function_varname("rr"),
      ref_tbl = tibble::tibble(
        label = c("plot_mod", "confint_betabinom"),
        reference = c("plot_mod", "confint_betabinom"),
        tabName = c("rank_regression", "confint_betabinom")
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
    r_distribution_arg(
      inputId = ns("distribution")
    ),
    conf_level_ui(
      id = ns("conf_level")
    ),
    r_direction_arg(
      inputId = ns("direction"),
      choices = c("x_on_y", "y_on_x")
    ),
    r_function_arg(
      "control",
      htmltools::pre("list()")
    ),
    r_function_arg(
      "options",
      htmltools::pre("list(conf_method = \"HC\")")
    )
  )
}

rank_regression_fun_server <- function(id, .values, estimate_cdf_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cdf_varname <- attr(estimate_cdf_r, "shinymetaVarname", exact = TRUE)

      output$placeholder <- shiny::renderUI({
        glue::glue(
          '
          x = {x},
          distribution = {distribution},
          conf_level = {conf_level}
          ',
          x = cdf_varname,
          distribution = input$distribution,
          conf_level = conf_level_return$conf_level_r()
        )
      })

      shiny::outputOptions(
        output,
        "placeholder",
        suspendWhenHidden = FALSE
      )

      output$x <- shiny::renderUI({
        varname_link(
          tabName = "probability_estimation",
          varname = cdf_varname
        )
      })

      distribution_r <- shiny::reactive({
        input$distribution %||% "weibull"
      })

      # For qf_incompatible_distribution
      .values$rank_regression_distribution_id <- "distribution"
      .values$rank_regression_session <- session

      direction_r <- shiny::reactive({
        input$direction %||% "x_on_y"
      })

      conf_method_r <- shiny::reactive({
        # input$conf_method %||% "HC"
        "HC"
      })

      rank_regression_r <- shinymeta::metaReactive({
        rank_regression(
          x = ..(estimate_cdf_r()),
          distribution = ..(distribution_r()),
          conf_level = ..(conf_level_return$conf_level_r()),
          direction = ..(direction_r()),
          control = list(),
          options = list(conf_method = ..(conf_method_r()))
        )
      }, varname = "rr")

      conf_level_return <- conf_level_server(
        id = "conf_level",
        .values = .values
      )

      return_list <- list(
        rank_regression_r = rank_regression_r
      )

      return(return_list)
    }
  )
}
