reliability_data_ui <- function(id) {
  ns <- shiny::NS(id)

  input_datasets <- c("alloy", "shock", "voltage")

  shiny::fluidRow(
    shiny::column(
      width = 6,
      reliability_data_fun_ui(
        id = ns("reliability_data_fun")
      )
    ),
    shiny::column(
      width = 6,
      code_box_ui(
        id = ns("code")
      ),
      bs4Dash::box(
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        title = "Result",
        DT::dataTableOutput(
          outputId = ns("result")
        )
      )
    )
  )
}

reliability_data_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$result <- DT::renderDataTable({
        DT::datatable(fun_return$reliability_data_r())
      })

      fun_return <- reliability_data_fun_server(
        id = "reliability_data_fun",
        .values = .values
      )

      code_box_server(
        id = "code",
        .values = .values,
        obj_r = fun_return$reliability_data_r
      )
    }
  )
}
