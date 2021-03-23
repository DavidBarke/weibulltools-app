gs_rank_regression_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("distribution")
    ),
    purrr::map(distributions(include3 = TRUE), function(dist) {
      shiny::conditionalPanel(
        condition = glue::glue('input.distribution == "{dist}"'),
        r_code(
          'rr <- rank_regression(',
          '  shock_cdf',
          glue::glue('  distribution = "{dist}"'),
          ')',
          '',
          'rr'
        ),
        ns = ns
      )
    }),
    list_result_ui(
      id = ns("rank_regression")
    )
  )
}



gs_rank_regression_server <- function(id,
                                      .values,
                                      estimate_cdf_r,
                                      distribution_r
) {
  shiny::moduleServer(
    id = id,
    function(input, output, session) {

      ns <- session$ns

      output$distribution <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("distribution"),
          label = "Distribution",
          choices = compatible_distributions(distribution_r())
        )
      })

      # This reactive is needed because the selectInput does not update when
      # the tabPanel is hidden
      safe_distribution_r <- shiny::reactive({
        if ((input$distribution %||% FALSE) %in% distribution_r()) {
          input$distribution
        } else {
          distribution_r()
        }
      })

      rank_regression_r <- shiny::reactive({
        weibulltools::rank_regression(
          estimate_cdf_r()("johnson"),
          distribution = safe_distribution_r()
        )
      })

      list_result_server(
        id = "rank_regression",
        .values = .values,
        obj_r = rank_regression_r
      )

      return_list <- list(
        rank_regression_r = rank_regression_r
      )

      return(return_list)
    }
  )
}
