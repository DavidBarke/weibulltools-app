rank_regression_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "rr <- rank_regression",
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

rank_regression_fun_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      rank_regression_r <- shinymeta::metaReactive({
        cdf_tbl <- reliability_data(shock, x = distance, status = status)

        rank_regression(
          cdf_tbl,
          distribution = ..(shiny::req(input$distribution)),
          conf_level = ..(shiny::req(input$conf_level))
        )
      }, varname = "rr")

      return_list <- list(
        rank_regression_r = rank_regression_r
      )

      return(return_list)
    }
  )
}
