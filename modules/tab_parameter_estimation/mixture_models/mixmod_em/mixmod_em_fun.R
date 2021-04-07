mixmod_em_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "mixmod_em",
    varname = ref_dropdown(
      id = ns("ref_dropdown"),
      varname = r_function_varname("mix_mod_em"),
      ref_tbl = tibble::tibble(
        label = c("plot_prob", "plot_mod"),
        reference = c("plot_prob", "plot_mod"),
        tabName = c("mixmod_em", "mixmod_em")
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

      rd_varname <- attr(reliability_data_r, "shinymetaVarname", exact = TRUE)

      conf_level_r <- shiny::reactive({
        input$conf_level %||% 0.95
      })

      k_r <- shiny::reactive({
        input$k %||% 2
      })

      # For qf_incompatible_distribution
      .values$mixmod_em_k_id <- "k"
      .values$mixmod_em_session <- session

      n_iter_r <- shiny::reactive({
        input$n_iter %||% 100L
      })

      conv_limit_r <- shiny::reactive({
        input$conv_limit %||% 1e-06
      })

      diff_loglik_r <- shiny::reactive({
        input$diff_loglik %||% 0.01
      })

      output$placeholder <- shiny::renderUI({
        glue::glue(
          '
          x = {x},
          conf_level = {conf_level},
          k = {k},
          n_iter = {n_iter},
          conv_limit = {conv_limit},
          diff_loglik = {diff_loglik}
          ',
          x = rd_varname,
          conf_level = conf_level_r(),
          k = k_r(),
          n_iter = n_iter_r(),
          conv_limit = conv_limit_r(),
          diff_loglik = diff_loglik_r()
        )
      })

      shiny::outputOptions(
        output,
        "placeholder",
        suspendWhenHidden = FALSE
      )

      output$x <- shiny::renderUI({
        varname_link(
          tabName = "reliability_data",
          varname = rd_varname
        )
      })

      mixmod_em_r <- shinymeta::metaReactive({
        mixmod_em(
          x = ..(reliability_data_r()),
          conf_level = ..(conf_level_r()),
          k = ..(k_r()),
          n_iter = ..(n_iter_r()),
          conv_limit = ..(conv_limit_r()),
          diff_loglik = ..(diff_loglik_r())
        )
      }, varname = "mix_mod_em")

      return_list <- list(
        mixmod_em_r = mixmod_em_r
      )

      return(return_list)
    }
  )
}
