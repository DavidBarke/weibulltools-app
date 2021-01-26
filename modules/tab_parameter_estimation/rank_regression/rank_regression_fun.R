rank_regression_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "rank_regression",
    varname = ref_dropdown_ui(
      id = ns("ref_dropdown"),
      varname = "rr",
      references = c("plot_mod", "confint_betabinom")
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
    )
  )
}

rank_regression_fun_server <- function(id, .values, estimate_cdf_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      ref_dropdown_server(
        id = "ref_dropdown",
        .values = .values,
        tabNames = c(
          plot_mod = "rank_regression",
          confint_betabinom = "confint_betabinom"
        )
      )

      output$x <- shiny::renderUI({
        varname_link_ui(
          id = ns("varname_link_probability_estimation"),
          name = attr(estimate_cdf_r, "shinymetaVarname", exact = TRUE)
        )
      })

      varname_link_server(
        id = "varname_link_probability_estimation",
        .values = .values,
        tabName = "probability_estimation"
      )

      output$conf_level <- shiny::renderUI({
        if (input$distribution %in% c("weibull", "weibull3")) {
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
