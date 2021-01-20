probability_estimation_result_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::tabBox(
    id = ns("tab_results"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    type = "tabs",
    side = "right",
    title = "Result",
    shiny::tabPanel(
      title = "estimate_cdf",
      estimate_cdf_result_ui(
        id = ns("estimate_cdf_result")
      )
    ),
    shiny::tabPanel(
      title = "plot_prob",
      plot_result_ui(
        id = ns("plot_prob_result")
      )
    )
  )
}

probability_estimation_result_server <- function(id,
                                                 .values,
                                                 estimate_cdf_r,
                                                 plot_prob_r
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      estimate_cdf_result_server(
        id = "estimate_cdf_result",
        .values = .values,
        estimate_cdf_r
      )

      plot_result_server(
        id = "plot_prob_result",
        .values = .values,
        p_obj_r = plot_prob_r
      )
    }
  )
}
