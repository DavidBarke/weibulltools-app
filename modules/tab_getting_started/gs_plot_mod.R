gs_plot_mod_ui <- function(id) {
  ns <- shiny::NS(id)

  text_box(
    title = "Visualization of Parametric Lifetime Distributions",
    htmltools::includeMarkdown("getting_started/plot_mod.md"),
    bs4Dash::tabBox(
      id = ns("tabs_plot_mod"),
      width = 12,
      shiny::tabPanel(
        title = "rank_regression",
        r_code(
          'p_weib %>%',
          '  plot_mod(',
          '    rr',
          '    title_trace = "Rank Regression"',
          '  )'
        ),
        plotly::plotlyOutput(
          outputId = ns("plot_mod_rr")
        )
      ),
      shiny::tabPanel(
        title = "ml_estimation",
        r_code(
          'p_weib %>%',
          '  plot_mod(',
          '    mle,',
          '    title_trace = "ML Estimation',
          '  )'
        ),
        plotly::plotlyOutput(
          outputId = ns("plot_mod_mle")
        )
      )
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

      plot_mod_rr_r <- shiny::reactive({
        plot_prob_r() %>%
          plot_mod(
            rank_regression_r(),
            title_trace = "Rank Regression"
          )
      })

      plot_mod_mle_r <- shiny::reactive({
        plot_prob_r() %>%
          plot_mod(
            ml_estimation_r(),
            title_trace = "ML Estimation"
          )
      })

      output$plot_mod_rr <- plotly::renderPlotly({
        plot_mod_rr_r()
      })

      output$plot_mod_mle <- plotly::renderPlotly({
        plot_mod_mle_r()
      })
    }
  )
}
