gs_probability_plotting_ui <- function(id) {
  ns <- shiny::NS(id)

  text_box(
    title = "Probability Plotting",
    htmltools::includeMarkdown("getting_started/probability_plotting.md"),
    shiny::selectInput(
      inputId = ns("plot_prob_distribution"),
      label = "Distribution",
      choices = distributions(include_thres = FALSE)
    ),
    purrr::map(distributions(), function(dist) {
      shiny::conditionalPanel(
        condition = glue::glue('input.plot_prob_distribution == "{dist}"'),
        r_code(glue::glue('p_weib <- plot_prob(shock_cdf, "{dist}")')),
        ns = ns
      )
    }),
    plotly::plotlyOutput(
      outputId = ns("plot_prob")
    )
  )
}

gs_probability_plotting_server <- function(id, .values, estimate_cdf_r) {
  shiny::moduleServer(
    id = id,
    function(input, output, session) {

      ns <- session$ns

      distribution_r <- shiny::reactive({
        input$plot_prob_distribution
      })

      plot_prob_r <- shiny::reactive({
        weibulltools::plot_prob(
          estimate_cdf_r()("johnson"),
          distribution = distribution_r()
        )
      })

      output$plot_prob <- plotly::renderPlotly({
        plot_prob_r()
      })

      return_list <- list(
        distribution_r = distribution_r,
        plot_prob_r = plot_prob_r
      )

      return(return_list)
    }
  )
}
