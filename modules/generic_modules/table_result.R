table_result_ui <- function(id) {
  ns <- shiny::NS(id)

  DT::dataTableOutput(
    outputId = ns("table")
  ) %>% shinycssloaders::withSpinner()
}

table_result_server <- function(id, .values, obj_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$table <- DT::renderDataTable({
        DT::datatable(obj_r())
      })
    }
  )
}
