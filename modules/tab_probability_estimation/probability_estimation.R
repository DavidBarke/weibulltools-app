probability_estimation_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      probability_estimation_fun_ui(
        id = ns("probability_estimation_fun")
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

      fun_return <- probability_estimation_fun_server(
        id = "probability_estimation_fun",
        .values = .values
      )

      code_box_server(
        id = "code",
        .values = .values,
        obj_r = fun_return$estimate_cdf_r
      )
    }
  )
}
