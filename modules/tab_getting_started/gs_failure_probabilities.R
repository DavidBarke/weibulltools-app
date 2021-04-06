gs_failure_probabilities_ui <- function(id) {
  ns <- shiny::NS(id)

  text_box(
    title = "Estimation of Failure Probabilities",
    htmltools::includeMarkdown("getting_started/failure_probabilities.md"),
    bs4Dash::tabBox(
      id = "tabs_estimate_cdf",
      collapsible = FALSE,
      width = 12,
      shiny::tabPanel(
        title = "mr",
        r_code('shock_cdf <- estimate_cdf(shock_tbl, methods = "mr")'),
        DT::dataTableOutput(
          outputId = ns("estimate_cdf_mr")
        )
      ),
      shiny::tabPanel(
        title = "johnson",
        r_code('shock_cdf <- estimate_cdf(shock_tbl, methods = "johnson")'),
        DT::dataTableOutput(
          outputId = ns("estimate_cdf_johnson")
        )
      ),
      shiny::tabPanel(
        title = "kaplan",
        r_code('shock_cdf <- estimate_cdf(shock_tbl, methods = "kaplan")'),
        DT::dataTableOutput(
          outputId = ns("estimate_cdf_kaplan")
        )
      ),
      shiny::tabPanel(
        title = "nelson",
        r_code('shock_cdf <- estimate_cdf(shock_tbl, methods = "nelson")'),
        DT::dataTableOutput(
          outputId = ns("estimate_cdf_nelson")
        )
      )
    )
  )
}



gs_failure_probabilities_server <- function(id, .values, reliability_data_r) {
  shiny::moduleServer(
    id = id,
    function(input, output, session) {

      ns <- session$ns

      estimate_cdf_r <- shiny::reactive({
        function(method) {
          suppressMessages(
            weibulltools::estimate_cdf(
              reliability_data_r(),
              methods = method
            )
          )
        }
      })

      output$estimate_cdf_mr <- DT::renderDataTable({
        datatable_wrapper(estimate_cdf_r()("mr"))
      })

      output$estimate_cdf_johnson <- DT::renderDataTable({
        datatable_wrapper(estimate_cdf_r()("johnson"))
      })

      output$estimate_cdf_kaplan <- DT::renderDataTable({
        datatable_wrapper(estimate_cdf_r()("kaplan"))
      })

      output$estimate_cdf_nelson <- DT::renderDataTable({
        datatable_wrapper(estimate_cdf_r()("nelson"))
      })

      return_list <- list(
        estimate_cdf_r = estimate_cdf_r
      )

      return(return_list)
    }
  )
}
