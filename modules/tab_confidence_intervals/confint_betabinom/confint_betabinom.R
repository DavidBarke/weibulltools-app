confint_betabinom_ui <- function(id) {
  ns <- shiny::NS(id)

  conf_name <- "conf_bb"

  shiny::fluidRow(
    shiny::column(
      width = 6,
      bs4Dash::box(
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        title = "Beta Binomial Confidence Bounds",
        confint_betabinom_fun_ui(
          id = ns("confint_betabinom_fun"),
          conf_name = conf_name
        ),
        plot_conf_fun_ui(
          id = ns("plot_conf_fun"),
          plot_name = "p_prob",
          conf_name = conf_name
        )
      )
    ),
    shiny::column(
      width = 6,
      bs4Dash::tabBox(
        width = NULL,
        id = ns("tabs_code"),
        solidHeader = TRUE,
        status = "primary",
        side = "right",
        type = "tabs",
        title = "Code",
        code_tab_ui(
          id = ns("confint_betabinom_code"),
          title = "confint_betabinom"
        ),
        code_tab_ui(
          id = ns("plot_conf_code"),
          title = "plot_conf"
        )
      ),
      bs4Dash::tabBox(
        width = NULL,
        id = ns("tabs_result"),
        solidHeader = TRUE,
        status = "primary",
        side = "right",
        type = "tabs",
        title = "Result",
        shiny::tabPanel(
          title = "confint_betabinom",
          table_result_ui(
            id = ns("confint_betabinom_result")
          )
        ),
        shiny::tabPanel(
          title = "plot_conf",
          plot_result_ui(
            id = ns("plot_conf_result")
          )
        )
      )
    )
  )
}

confint_betabinom_server <- function(id,
                                     .values,
                                     rank_regression_r,
                                     plot_prob_r
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      shiny::observeEvent(input$tabs_code, {
        shiny::updateTabsetPanel(
          inputId = "tabs_result",
          selected = input$tabs_code
        )
      })

      shiny::observeEvent(input$tabs_result, {
        shiny::updateTabsetPanel(
          inputId = "tabs_code",
          selected = input$tabs_result
        )
      })

      conf_bb_return <- confint_betabinom_fun_server(
        id = "confint_betabinom_fun",
        .values = .values,
        rank_regression_r = rank_regression_r
      )

      plot_conf_return <- plot_conf_fun_server(
        id = "plot_conf_fun",
        .values = .values,
        conf_r = conf_bb_return$confint_betabinom_r,
        plot_prob_r = plot_prob_r
      )

      code_tab_server(
        id = "confint_betabinom_code",
        .values = .values,
        conf_bb_return$confint_betabinom_r
      )

      code_tab_server(
        id = "plot_conf_code",
        .values = .values,
        obj_r = plot_conf_return$plot_conf_r
      )

      table_result_server(
        id = "confint_betabinom_result",
        .values = .values,
        obj_r = conf_bb_return$confint_betabinom_r
      )

      plot_result_server(
        id = "plot_conf_result",
        .values = .values,
        p_obj_r = plot_conf_return$plot_conf_r
      )
    }
  )
}
