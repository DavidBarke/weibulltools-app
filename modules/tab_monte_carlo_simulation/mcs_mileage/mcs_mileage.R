mcs_mileage_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      bs4Dash::box(
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        title = "Simulation of Mileages",
        mcs_mileage_fun_ui(
          id = ns("mcs_mileage_fun")
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
        list_result_ui(
          id = ns("result")
        )
      )
    )
  )
}

mcs_mileage_server <- function(id,
                               .values,
                               mcs_mileage_data_r
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      mcs_mileage_return <- mcs_mileage_fun_server(
        id = "mcs_mileage_fun",
        .values = .values,
        mcs_mileage_data_r = mcs_mileage_data_r
      )

      code_box_server(
        id = "code",
        .values = .values,
        obj_r = mcs_mileage_return$mcs_mileage_r
      )

      list_result_server(
        id = "result",
        .values = .values,
        obj_r = mcs_mileage_return$mcs_mileage_r
      )

      return_list <- list(
        mcs_mileage_r = mcs_mileage_return$mcs_mileage_r
      )

      return(return_list)
    }
  )
}
