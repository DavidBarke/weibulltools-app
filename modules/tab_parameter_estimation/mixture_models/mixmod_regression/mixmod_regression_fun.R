mixmod_regression_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "mixmod_regression",
    varname = ref_dropdown_ui(
      id = ns("ref_dropdown"),
      varname = r_function_varname("mix_mod_regression"),
      references = c(
        "plot_prob",
        "plot_mod"
      )
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

      ref_dropdown_server(
        id = "ref_dropdown",
        .values = .values,
        tabNames = c(
          plot_prob = "mixmod_regression",
          plot_mod = "mixmod_regression"
        )
      )

      cdf_varname <- attr(estimate_cdf_r, "shinymetaVarname", exact = TRUE)

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

      conf_level_r <- shiny::reactive({
        as.numeric(input$conf_level %||% 0.95)
      })

      mixmod_regression_r <- shinymeta::metaReactive({
        mixmod_regression(
          x = ..(estimate_cdf_r()),
          distribution = ..(shiny::req(input$distribution)),
          conf_level = ..(conf_level_r()),
          k = ..(input$k %||% 2),
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
