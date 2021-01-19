estimate_cdf_result_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::tabBox(
    id = ns("tab_box"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    type = "tabs",
    side = "right",
    title = "Result",
    shiny::tabPanel(
      title = "Data",
      DT::dataTableOutput(
        outputId = ns("data")
      )
    ),
    shiny::tabPanel(
      title = "Plot"
    )
  )
}

estimate_cdf_result_server <- function(id, .values, estimate_cdf_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$data <- DT::renderDataTable({
        estimate_cdf_r()
      })
    }
  )
}
