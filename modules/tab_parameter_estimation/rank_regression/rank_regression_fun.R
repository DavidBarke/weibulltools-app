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
    r_function_arg(
      "conf_level",
      shiny::uiOutput(
        outputId = ns("conf_level")
      )
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
          conf_level = input$conf_level
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

      output$conf_level <- shiny::renderUI({
        if (input$distribution %in% c("weibull", "weibull3")) {
          preSelectInput(
            inputId = ns("conf_level_weib"),
            label = NULL,
            choices = c(0.9, 0.95, 0.99),
            width = "100%"
          )
        } else {
          preNumericInput(
            inputId = ns("conf_level"),
            label = NULL,
            value = 0.95,
            min = 0,
            max = 1,
            step = 0.01,
            width = "100%"
          )
        }
      })

      distribution_r <- shiny::reactive({
        input$distribution %||% "weibull"
      })

      # For qf_incompatible_distribution
      .values$rank_regression_distribution_id <- "distribution"
      .values$rank_regression_session <- session

      conf_level_r <- shiny::reactive({
        if (distribution_r() %in% c("weibull", "weibull3")) {
          as.numeric(input$conf_level_weib %||% 0.95)
        } else {
          as.numeric(input$conf_level %||% 0.95)
        }
      })

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
          conf_level = ..(conf_level_r()),
          direction = ..(direction_r()),
          options = list(conf_method = ..(conf_method_r()))
        )
      }, varname = "rr")

      return_list <- list(
        rank_regression_r = rank_regression_r
      )

      return(return_list)
    }
  )
}
