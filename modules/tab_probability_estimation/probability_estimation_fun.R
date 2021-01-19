probability_estimation_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  cdf_estimation_name <- "cdf_tbl"

  bs4Dash::box(
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    title = "Non-Parametric Failure Probabilities",
    htmltools::p(
      "Functions for estimation and visualization of failure probabilities"
    ),
    estimate_cdf_fun_ui(
      id = ns("estimate_cdf_fun"),
      cdf_estimation_name = cdf_estimation_name
    ),
    htmltools::br(),
    plot_prob_fun_ui(
      id = ns("plot_prob_fun"),
      cdf_estimation_name = cdf_estimation_name
    )
  )
}

probability_estimation_fun_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      est_cdf_return <- estimate_cdf_fun_server(
        id = "estimate_cdf_fun",
        .values = .values
      )

      plot_prob_return <- plot_prob_fun_server(
        id = "plot_prob_fun",
        .values = .values,
        estimate_cdf_r = est_cdf_return$estimate_cdf_r
      )

      return_list <- list(
        estimate_cdf_r = est_cdf_return$estimate_cdf_r,
        plot_prob_r = plot_prob_return$plot_prob_r
      )

      return(return_list)
    }
  )
}
