mcs_delay_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      bs4Dash::box(
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        title = "Simulation of Delays",
        htmltools::p(
          "Function for the simulation of delays as well as the adjustment of operating times"
        ),
        mcs_delay_fun_ui(
          id = ns("mcs_delay_fun")
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

mcs_delay_server <- function(id,
                             .values,
                             mcs_data_r
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      mcs_delay_return <- mcs_delay_fun_server(
        id = "mcs_delay_fun",
        .values = .values,
        mcs_data_r = mcs_data_r
      )

      code_box_server(
        id = "code",
        .values = .values,
        obj_r = mcs_delay_return$mcs_delay_r
      )

      list_result_server(
        id = "result",
        .values = .values,
        obj_r = mcs_delay_return$mcs_delay_r
      )

      return_list <- list(
        mcs_delay_r = mcs_delay_return$mcs_delay_r
      )

      return(return_list)
    }
  )
}
