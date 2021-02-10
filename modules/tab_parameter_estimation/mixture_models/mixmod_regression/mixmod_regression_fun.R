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
    r_function_arg(
      "conf_level",
      shiny::uiOutput(
        outputId = ns("conf_level")
      )
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

      conf_level_r <- shiny::reactive({
        as.numeric(input$conf_level %||% 0.95)
      })

      k_r <- shiny::reactive({
        input$k %||% 2
      })

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
          conf_level = conf_level_r(),
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

      output$conf_level <- shiny::renderUI({
        if (input$distribution == "weibull") {
          preSelectInput(
            inputId = ns("conf_level"),
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

      mixmod_regression_r <- shinymeta::metaReactive({
        mixmod_regression(
          x = ..(estimate_cdf_r()),
          distribution = ..(distribution_r()),
          conf_level = ..(conf_level_r()),
          k = ..(k_r()),
          control = segmented::seg.control()
        )
      }, varname = "mix_mod_regression")

      return_list <- list(
        mixmod_regression_r = mixmod_regression_r
      )

      return(return_list)
    }
  )
}
