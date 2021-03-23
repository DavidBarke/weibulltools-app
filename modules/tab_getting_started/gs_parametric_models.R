gs_parametric_models_ui <- function(id) {
  ns <- shiny::NS(id)

  text_box(
    title = "Estimation of Parametric Lifetime Distributions",
    htmltools::includeMarkdown("getting_started/parametric_models.md"),
    bs4Dash::tabBox(
      id = "tabs_parametric_models",
      collapsible = FALSE,
      width = 12,
      shiny::tabPanel(
        title = "rank_regression",
        gs_rank_regression_ui(
          id = ns("rank_regression")
        )
      ),
      shiny::tabPanel(
        title = "ml_estimation",
        gs_ml_estimation_ui(
          id = ns("ml_estimation")
        )
      )
    )
  )
}



gs_parametric_models_server <- function(id,
                                        .values,
                                        reliability_data_r,
                                        estimate_cdf_r,
                                        distribution_r
) {
  shiny::moduleServer(
    id = id,
    function(input, output, session) {

      ns <- session$ns

      rank_regression_return <- gs_rank_regression_server(
        id = "rank_regression",
        .values = .values,
        estimate_cdf_r = estimate_cdf_r,
        distribution_r = distribution_r
      )

      ml_estimation_return <- gs_ml_estimation_server(
        id = "ml_estimation",
        .values = .values,
        reliability_data_r = reliability_data_r,
        distribution_r = distribution_r
      )

      return_list <- list(
        rank_regression_r = rank_regression_return$rank_regression_r,
        ml_estimation_r = ml_estimation_return$ml_estimation_r
      )
    }
  )
}
