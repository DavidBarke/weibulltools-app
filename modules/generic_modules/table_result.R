table_result_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    error_display_ui(
      id = ns("error_display")
    ),
    DT::dataTableOutput(
      outputId = ns("table")
    ) %>% shinycssloaders::withSpinner()
  )
}

table_result_server <- function(id, .values, obj_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$table <- DT::renderDataTable({
        if (!error_display_return$error_r()) {
          DT::datatable(obj_r(), class = "display compact")
        }
      })

      error_display_return <- error_display_server(
        id = "error_display",
        .values = .values,
        obj_r = obj_r
      )
    }
  )
}
