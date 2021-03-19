getting_started_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    text_box(
      title = "Introduction",
      htmltools::includeHTML("getting_started/introduction.html")
    ),
    text_box(
      title = "Reliability Data",
      htmltools::includeHTML("getting_started/reliability_data.html"),
      bs4Dash::tabBox(
        id = ns("tabs_reliability_data"),
        collapsible = FALSE,
        width = 12,
        shiny::tabPanel(
          title = "reliability_data(shock)",
          DT::dataTableOutput(
            outputId = ns("reliability_data")
          )
        ),
        shiny::tabPanel(
          title = "shock",
          DT::dataTableOutput(
            outputId = ns("shock")
          )
        )
      )
    ),
    text_box(
      title = "Estimation of Failure Probabilities",
      htmltools::includeHTML("getting_started/failure_probabilities.html"),
      bs4Dash::tabBox(
        id = "tabs_estimate_cdf",
        collapsible = FALSE,
        width = 12,
        shiny::tabPanel(
          title = "mr",
          DT::dataTableOutput(
            outputId = ns("estimate_cdf_mr")
          )
        ),
        shiny::tabPanel(
          title = "johnson",
          DT::dataTableOutput(
            outputId = ns("estimate_cdf_johnson")
          )
        ),
        shiny::tabPanel(
          title = "kaplan",
          DT::dataTableOutput(
            outputId = ns("estimate_cdf_kaplan")
          )
        ),
        shiny::tabPanel(
          title = "nelson",
          DT::dataTableOutput(
            outputId = ns("estimate_cdf_nelson")
          )
        )
      )
    ),
    text_box(
      title = "Probability Plotting",
      htmltools::includeHTML("getting_started/probability_plotting.html"),
      shiny::selectInput(
        inputId = ns("plot_prob_distribution"),
        label = "Distribution",
        choices = distributions(include3 = FALSE)
      ),
      plotly::plotlyOutput(
        outputId = ns("plot_prob")
      )
    )
  )
}

getting_started_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      reliability_data_r <- shiny::reactive({
        weibulltools::reliability_data(
          data = weibulltools::shock,
          x = distance,
          status = status
        )
      })

      output$reliability_data <- DT::renderDataTable({
        datatable_wrapper(reliability_data_r())
      })

      output$shock <- DT::renderDataTable({
        datatable_wrapper(weibulltools::shock)
      })

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

      plot_prob_r <- shiny::reactive({
        weibulltools::plot_prob(
          estimate_cdf_r()(c("mr", "johnson", "nelson", "kaplan")),
          distribution = input$plot_prob_distribution
        )
      })

      output$plot_prob <- plotly::renderPlotly({
        plot_prob_r()
      })
    }
  )
}
