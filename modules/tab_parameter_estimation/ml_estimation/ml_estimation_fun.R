ml_estimation_fun_ui <- function(id, model_name) {
  ns <- shiny::NS(id)

  r_function(
    name = "ml_estimation",
    varname = model_name,
    r_function_arg(
      "x"
    ),
    r_distribution_arg(
      inputId = ns("distribution")
    ),
    r_function_arg(
      "wts",
      "rep(1, nrow(x))"
    ),
    r_conf_level_arg(
      inputId = ns("conf_level")
    )
  )
}

ml_estimation_fun_server <- function(id, .values, reliability_data_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      ml_estimation_r <- shinymeta::metaReactive({
        ml_estimation(
          x = ..(reliability_data_r()),
          distribution = ..(shiny::req(input$distribution)),
          conf_level = ..(shiny::req(input$conf_level))
        )
      }, varname = "mle")

      return_list <- list(
        ml_estimation_r = ml_estimation_r
      )

      return(return_list)
    }
  )
}
