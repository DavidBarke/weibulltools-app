gs_reliability_data_ui <- function(id) {
  ns <- shiny::NS(id)

  text_box(
    title = "Reliability Data",
    htmltools::includeMarkdown("getting_started/reliability_data.md"),
    r_code(
      "library(weibulltools)",
      "shock_tbl <- reliability_data(shock, x = distance, status = status)",
      "shock_tbl"
    ),
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
  )
}

gs_reliability_data_server <- function(id, .values) {
  shiny::moduleServer(
    id = id,
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

      return_list <- list(
        reliability_data_r = reliability_data_r
      )

      return(return_list)
    }
  )
}
