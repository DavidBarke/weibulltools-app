gs_plot_mod_ui <- function(id) {
  ns <- shiny::NS(id)

  text_box(
    title = "Visualization of Parametric Lifetime Distributions",
    htmltools::includeMarkdown("getting_started/plot_mod.md"),
    r_code(
      'p_weib %>%',
      '  plot_mod(',
      '    rr',
      '    title_trace = "Rank Regression"',
      '  ) %>%',
      '  plot_mod(',
      '    mle,',
      '    title_trace = "ML Estimation',
      '  )'
    ),
    plotly::plotlyOutput(
      outputId = ns("plot_mod")
    )
  )
}



gs_plot_mod_server <- function(id,
                               .values,
                               plot_prob_r,
                               rank_regression_r,
                               ml_estimation_r
) {
  shiny::moduleServer(
    id = id,
    function(input, output, session) {

      ns <- session$ns

      plot_mod_r <- shiny::reactive({
        plot_prob_r() %>%
          plot_mod(
            rank_regression_r(),
            title_trace = "Rank Regression"
          ) %>%
          plot_mod(
            ml_estimation_r(),
            title_trace = "ML Estimation"
          )
      })

      output$plot_mod <- plotly::renderPlotly({
        plot_mod_r()
      })
    }
  )
}
