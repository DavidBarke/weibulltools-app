confint_fisher_ui <- function(id) {
  ns <- shiny::NS(id)

  conf_name <- "conf_fisher"

  shiny::fluidRow(
    shiny::column(
      width = 6,
      bs4Dash::box(
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        title = "Fisher's Confidence Bounds",
        confint_fisher_fun_ui(
          id = ns("confint_fisher_fun"),
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
          id = ns("confint_fisher_code"),
          title = "confint_fisher"
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
          title = "confint_fisher",
          table_result_ui(
            id = ns("confint_fisher_result")
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

confint_fisher_server <- function(id, .values) {
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

      reliability_data_r <- shinymeta::metaReactive({
        reliability_data(data = shock, x = distance, status = status)
      }, varname = "rel_tbl")

      estimate_cdf_r <- shinymeta::metaReactive({
        estimate_cdf(x = ..(reliability_data_r()), methods = "johnson")
      }, varname = "cdf_tbl")

      ml_estimation_r <- shinymeta::metaReactive({
        ml_estimation(x = ..(reliability_data_r()), distribution = "weibull",
                        conf_level = 0.95)
      }, varname = "mle")

      conf_fisher_return <- confint_fisher_fun_server(
        id = "confint_fisher_fun",
        .values = .values,
        ml_estimation_r = ml_estimation_r
      )

      plot_prob_r <- shinymeta::metaReactive({
        plot_prob(
          ..(estimate_cdf_r()),
          distribution = "weibull"
        )
      }, varname = "p_prob")

      plot_conf_return <- plot_conf_fun_server(
        id = "plot_conf_fun",
        .values = .values,
        conf_r = conf_fisher_return$confint_fisher_r,
        plot_prob_r = plot_prob_r
      )

      code_tab_server(
        id = "confint_fisher_code",
        .values = .values,
        conf_fisher_return$confint_fisher_r
      )

      code_tab_server(
        id = "plot_conf_code",
        .values = .values,
        obj_r = plot_conf_return$plot_conf_r
      )

      table_result_server(
        id = "confint_fisher_result",
        .values = .values,
        obj_r = conf_fisher_return$confint_fisher_r
      )

      plot_result_server(
        id = "plot_conf_result",
        .values = .values,
        p_obj_r = plot_conf_return$plot_conf_r
      )
    }
  )
}
