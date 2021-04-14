mixmod_regression_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "mixmod_regression",
    varname = ref_dropdown(
      varname = r_function_varname("mix_mod_regression"),
      ref_tbl = tibble::tibble(
        label = c("plot_prob", "plot_mod"),
        reference = c("plot_prob", "plot_mod"),
        tabName = c("mixmod_regression", "mixmod_regression")
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
      name = "distribution",
      preSelectInput(
        inputId = ns("distribution"),
        label = NULL,
        choices = c("weibull", "lognormal", "loglogistic")
      )
    ),
    conf_level_ui(
      id = ns("conf_level")
    ),
    r_k_arg(
      inputId = ns("k")
    ),
    r_function_arg(
      "control",
      htmltools::pre("segmented::seg.control()")
    )
  )
}

mixmod_regression_fun_server <- function(id, .values, estimate_cdf_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cdf_varname <- attr(estimate_cdf_r, "shinymetaVarname", exact = TRUE)

      distribution_r <- shiny::reactive({
        input$distribution %||% "weibull"
      })

      k_r <- shiny::reactive({
        k <- input$k %||% 2

        if (k < 1) return(1)

        as.integer(k)
      })

      # For qf_incompatible_distribution
      .values$mixmod_regression_k_id <- "k"
      .values$mixmod_regression_session <- session

      output$placeholder <- shiny::renderUI({
        glue::glue(
          '
          x = {x},
          distribution = "{distribution}",
          conf_level = {conf_level},
          k = {k},
          control = segmented::seg.control()
          ',
          x = cdf_varname,
          distribution = distribution_r(),
          conf_level = conf_level_return$conf_level_r(),
          k = k_r()
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

      mixmod_regression_r <- shinymeta::metaReactive({
        mixmod_regression(
          x = ..(estimate_cdf_r()),
          distribution = ..(distribution_r()),
          conf_level = ..(conf_level_return$conf_level_r()),
          k = ..(k_r()),
          control = segmented::seg.control()
        )
      }, varname = "mix_mod_regression")

      conf_level_return <- conf_level_server(
        id = "conf_level",
        .values = .values
      )

      return_list <- list(
        mixmod_regression_r = mixmod_regression_r
      )

      return(return_list)
    }
  )
}
