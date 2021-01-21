rank_regression_fun_ui <- function(id, model_name) {
  ns <- shiny::NS(id)

  r_function(
    name = "rank_regression",
    varname = model_name,
    r_function_arg(
      "x"
    ),
    r_distribution_arg(
      inputId = ns("distribution")
    ),
    r_conf_level_arg(
      inputId = ns("conf_level")
    )
  )
}

rank_regression_fun_server <- function(id, .values, estimate_cdf_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      weibull_level <- c(0.9, 0.95, 0.99)
      shiny::observeEvent(input$conf_level, {
        if (input$distribution %in% c("weibull", "weibull3")) {
          if (!input$conf_level %in% weibull_level) {
            abs_diff <- abs(weibull_level - input$conf_level)
            nearest <- weibull_level[abs_diff == min(abs_diff)][1]

            shiny::updateNumericInput(
              inputId = "conf_level",
              value = nearest
            )
          }
        }
      })

      conf_level_r <- shiny::reactive({
        if (input$distribution %in% c("weibull", "weibull3")) {
          shiny::req(input$conf_level %in% weibull_level)
        }

        shiny::req(input$conf_level)
      })

      rank_regression_r <- shinymeta::metaReactive({
        rank_regression(
          ..(estimate_cdf_r()),
          distribution = ..(shiny::req(input$distribution)),
          conf_level = ..(conf_level_r())
        )
      }, varname = "rr")

      return_list <- list(
        rank_regression_r = rank_regression_r
      )

      return(return_list)
    }
  )
}
