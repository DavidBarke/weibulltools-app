reliability_data_ui <- function(id) {
  ns <- shiny::NS(id)

  input_datasets <- c("alloy", "shock", "voltage")

  shiny::fluidRow(
    shiny::column(
      width = 6,
      reliability_data_fun_ui(
        id = ns("reliability_data_fun")
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
    ),
    shiny::column(
      width = 6,
      bs4Dash::box(
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        title = "Datasets",
        shiny::selectInput(
          inputId = ns("input_data"),
          label = NULL,
          choices = input_datasets,
          width = "100%"
        ),
        DT::dataTableOutput(
          outputId = ns("input_data_table")
        )
      ),
      code_box_ui(
        id = ns("code")
      )
    )
  )
}

reliability_data_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      input_data_r <- shiny::reactive({
        get(fun_return$data_r())
      })

      output$input_data_table <- DT::renderDataTable({
        DT::datatable(input_data_r())
      })

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
