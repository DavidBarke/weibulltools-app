mcs_mileage_data_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      bs4Dash::box(
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        title = "MCS Mileage Data",
        mcs_mileage_data_fun_ui(
          id = ns("mcs_mileage_data_fun")
        )
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
        table_result_ui(
          id = ns("result")
        )
      )
    )
  )
}

mcs_mileage_data_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      rd_return <- mcs_mileage_data_fun_server(
        id = "mcs_mileage_data_fun",
        .values = .values
      )

      code_box_server(
        id = "code",
        .values = .values,
        obj_r = rd_return$mcs_mileage_data_r
      )

      table_result_server(
        id = "result",
        .values = .values,
        obj_r = rd_return$mcs_mileage_data_r
      )

      return_list <- list(
        mcs_mileage_data_r = rd_return$mcs_mileage_data_r
      )

      return(return_list)
    }
  )
}
