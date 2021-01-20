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
      bs4Dash::tabBox(
        id = ns("code_tabs"),
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        type = "tabs",
        side = "right",
        title = "Code",
        code_tab_ui(
          id = ns("estimate_cdf_code"),
          title = "estimate_cdf"
        ),
        code_tab_ui(
          id = ns("plot_prob_code"),
          title = "plot_prob"
        )
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

      code_tab_server(
        id = "estimate_cdf_code",
        .values = .values,
        obj_r = fun_return$estimate_cdf_r
      )

      code_tab_server(
        id = "plot_prob_code",
        .values = .values,
        obj_r = fun_return$plot_prob_r
      )
    }
  )
}
