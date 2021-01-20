ml_estimation_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      bs4Dash::box(
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        title = "ML Estimation",
        ml_estimation_fun_ui(
          id = ns("ml_estimation_fun")
        ),
        plot_mod_fun_ui(
          id = ns("plot_mod_fun")
        )
      )
    )
  )
}

ml_estimation_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      ml_estimation_fun_server(
        id = "ml_estimation_fun",
        .values = .values
      )

      plot_mod_fun_server(
        id = "plot_mod_fun",
        .values = .values
      )
    }
  )
}
