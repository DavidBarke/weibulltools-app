cdf_estimation_name <- "cdf_tbl"

probability_estimation_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      bs4Dash::box(
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        title = "Non-Parametric Failure Probabilities",
        htmltools::p(
          "Functions for estimation and visualization of failure probabilities"
        ),
        estimate_cdf_fun_ui(
          id = ns("estimate_cdf_fun")
        ),
        htmltools::br(),
        plot_prob_fun_ui(
          id = ns("plot_prob_fun"),
          cdf_estimation_name = cdf_estimation_name
        )
      )
    ),
    shiny::column(
      width = 6,
      bs4Dash::tabBox(
        id = ns("tabs_code"),
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
      ),
      bs4Dash::tabBox(
        id = ns("tabs_result"),
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        type = "tabs",
        side = "right",
        title = "Result",
        shiny::tabPanel(
          title = "estimate_cdf",
          table_result_ui(
            id = ns("estimate_cdf_result")
          )
        ),
        shiny::tabPanel(
          title = "plot_prob",
          plot_result_ui(
            id = ns("plot_prob_result")
          )
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

      shiny::observeEvent(input$tabs_code, {
        shiny::updateTabsetPanel(
          session = session,
          inputId = "tabs_result",
          selected = input$tabs_code
        )
      })

      shiny::observeEvent(input$tabs_result, {
        shiny::updateTabsetPanel(
          session = session,
          inputId = "tabs_code",
          selected = input$tabs_result
        )
      })

      estimate_cdf_return <- estimate_cdf_fun_server(
        id = "estimate_cdf_fun",
        .values = .values,
        cdf_estimation_name = cdf_estimation_name
      )

      plot_prob_return <- plot_prob_fun_server(
        id = "plot_prob_fun",
        .values = .values,
        cdf_estimation_name = cdf_estimation_name,
        estimate_cdf_r = estimate_cdf_return$estimate_cdf_r
      )

      code_tab_server(
        id = "estimate_cdf_code",
        .values = .values,
        obj_r = estimate_cdf_return$estimate_cdf_r
      )

      code_tab_server(
        id = "plot_prob_code",
        .values = .values,
        obj_r = plot_prob_return$plot_prob_r
      )

      table_result_server(
        id = "estimate_cdf_result",
        .values = .values,
        obj_r = estimate_cdf_return$estimate_cdf_r
      )

      plot_result_server(
        id = "plot_prob_result",
        .values = .values,
        p_obj_r = plot_prob_return$plot_prob_r
      )
    }
  )
}
