plot_prob_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "plot_prob",
    varname = ref_dropdown_ui(
      id = ns("ref_dropdown"),
      varname = r_function_varname("p_prob"),
      references = c(
        ml_estimation = "ML Estimation: plot_mod",
        rank_regression = "Rank Regression: plot_mod",
        confint_betabinom = "Beta Binomial CI: plot_conf",
        confint_fisher = "Fisher CI: plot_conf"
      )
    ),
    r_function_arg(
      "x",
      shiny::uiOutput(
        outputId = ns("x"),
        container = htmltools::pre
      )
    ),
    r_distribution_arg(
      inputId = ns("distribution"),
      include3 = FALSE
    ),
    r_text_arg(
      name = "title_main",
      inputId = ns("title_main"),
      value = "Probability Plot"
    ),
    r_text_arg(
      name = "title_x",
      inputId = ns("title_x"),
      value = "Characteristic"
    ),
    r_text_arg(
      name = "title_y",
      inputId = ns("title_y"),
      value = "Unreliability"
    ),
    r_text_arg(
      name = "title_trace",
      inputId = ns("title_trace"),
      value = "Sample"
    ),
    r_plot_method_arg(
      inputId = ns("plot_method")
    )
  )
}

plot_prob_fun_server <- function(id, .values, estimate_cdf_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      ref_dropdown_server(
        id = "ref_dropdown",
        .values = .values,
        tabNames = c(
          ml_estimation = "ml_estimation",
          rank_regression = "rank_regression",
          confint_betabinom = "confint_betabinom",
          confint_fisher = "confint_fisher"
        )
      )

      cdf_varname <- attr(estimate_cdf_r, "shinymetaVarname", exact = TRUE)

      output$x <- shiny::renderUI({
        varname_link(
          tabName = NULL,
          varname = cdf_varname
        )
      })

      plot_prob_r <- shinymeta::metaReactive({
        plot_prob(
          x = ..(estimate_cdf_r()),
          distribution = ..(replace_comma(input$distribution %||% "weibull")),
          title_main = ..(replace_comma(input$title_main %||% "Probability Plot")),
          title_x = ..(replace_comma(input$title_x %||% "Characteristic")),
          title_y = ..(replace_comma(input$title_y %||% "Unreliability")),
          title_trace = ..(replace_comma(input$title_trace %||% "Sample")),
          plot_method = ..(replace_comma(input$plot_method %||% "plotly"))
        )
      }, varname = "p_prob")

      return_list <- list(
        plot_prob_r = plot_prob_r
      )

      return(return_list)
    }
  )
}
