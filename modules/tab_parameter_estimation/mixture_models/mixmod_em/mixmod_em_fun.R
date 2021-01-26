mixmod_em_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "mixmod_em",
    varname = "mix_mod_em",
    r_function_arg(
      "x",
      shiny::uiOutput(
        outputId = ns("x"),
        container = htmltools::pre
      )
    ),
    r_function_arg(
      "distribution",
      htmltools::pre("weibull")
    ),
    r_conf_level_arg(
      inputId = ns("conf_level")
    ),
    r_k_arg(
      inputId = ns("k")
    ),
    r_function_arg(
      "method",
      htmltools::pre("EM")
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

      output$x <- shiny::renderUI({
        varname_link_ui(
          id = ns("varname_link_reliability_data"),
          name = attr(reliability_data_r, "shinymetaVarname", exact = TRUE)
        )
      })

      varname_link_server(
        id = "varname_link_reliability_data",
        .values = .values,
        tabName = "reliability_data"
      )

      mixmod_em_r <- shinymeta::metaReactive({
        mixmod_em(
          x = ..(reliability_data_r()),
          conf_level = ..(input$conf_level %||% 0.95),
          k = ..(input$k %||% 2),
          n_iter = ..(input$n_iter %||% 100L),
          conv_limit = ..(input$conv_limit %||% 1e-06),
          diff_loglik = ..(input$diff_loglik %||% 0.01)
        )
      }, varname = "mix_mod_em")

      return_list <- list(
        mixmod_em_r = mixmod_em_r
      )

      return(return_list)
    }
  )
}
