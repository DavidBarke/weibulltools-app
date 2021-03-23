gs_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    text_box(
      title = "Introduction",
      htmltools::includeMarkdown("getting_started/introduction.md")
    ),
    gs_reliability_data_ui(
      id = ns("reliability_data")
    ),
    gs_failure_probabilities_ui(
      id = ns("failure_probabilities")
    ),
    gs_probability_plotting_ui(
      id = ns("probability_plotting")
    ),
    gs_parametric_models_ui(
      id = ns("parametric_models")
    ),
    gs_plot_mod_ui(
      id = ns("plot_mod")
    )
  )
}

gs_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      reliability_data_return <- gs_reliability_data_server(
        id = "reliability_data",
        .values = .values
      )

      failure_probabilities_return <- gs_failure_probabilities_server(
        id = "failure_probabilities",
        .values = .values,
        reliability_data_r = reliability_data_return$reliability_data_r
      )

      probability_plotting_return <- gs_probability_plotting_server(
        id = "probability_plotting",
        .values = .values,
        estimate_cdf_r = failure_probabilities_return$estimate_cdf_r
      )

      parametric_models_return <- gs_parametric_models_server(
        id = "parametric_models",
        .values = .values,
        reliability_data_r = reliability_data_return$reliability_data_r,
        estimate_cdf_r = failure_probabilities_return$estimate_cdf_r,
        distribution_r = probability_plotting_return$distribution_r
      )

      gs_plot_mod_server(
        id = "plot_mod",
        .values = .values,
        plot_prob_r = probability_plotting_return$plot_prob_r,
        rank_regression_r = parametric_models_return$rank_regression_r,
        ml_estimation_r = parametric_models_return$ml_estimation_r
      )
    }
  )
}
