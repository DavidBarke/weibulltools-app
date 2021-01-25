mixmod_em_fun_ui <- function(id, model_name) {
  ns <- shiny::NS(id)

  r_function(
    name = "mixmod_em",
    varname = model_name,
    r_function_arg(
      "x"
    ),
    r_function_arg(
      "distribution",
      "weibull"
    ),
    r_conf_level_arg(
      inputId = ns("conf_level")
    ),
    r_k_arg(
      inputId = ns("k")
    ),
    r_function_arg(
      "method",
      "EM"
    ),
    r_n_iter_arg(
      inputId = ns("n_iter")
    ),
    r_conv_limit_arg(
      inputId = ns("conv_limit")
    ),
    r_diff_loglik_arg(
      inputId = ns("diff_loglik")
    )
  )
}

mixmod_em_fun_server <- function(id, .values, reliability_data_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      mixmod_em_r <- shinymeta::metaReactive({
        mixmod_em(
          x = ..(reliability_data_r()),
          conf_level = ..(shiny::req(input$conf_level)),
          k = ..(shiny::req(input$k)),
          n_iter = ..(shiny::req(input$n_iter)),
          conv_limit = ..(shiny::req(input$conv_limit)),
          diff_loglik = ..(shiny::req(input$diff_loglik))
        )
      }, varname = "mix_mod_em")

      return_list <- list(
        mixmod_em_r = mixmod_em_r
      )

      return(return_list)
    }
  )
}
