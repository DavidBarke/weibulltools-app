probability_estimation_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      estimate_cdf_fun_ui(
        id = ns("estimate_cdf_fun")
      ),
      estimate_cdf_result_ui(
        id = ns("estimate_cdf_result")
      )
    ),
    shiny::column(
      width = 6,
      code_box_ui(
        id = ns("code")
      )
    )
  )
}

probability_estimation_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      fun_return <- estimate_cdf_fun_server(
        id = "estimate_cdf_fun",
        .values = .values
      )

      estimate_cdf_result_server(
        id = "estimate_cdf_result",
        .values = .values,
        estimate_cdf_r = fun_return$estimate_cdf_r
      )

      code_box_server(
        id = "code",
        .values = .values,
        obj_r = fun_return$estimate_cdf_r
      )
    }
  )
}
