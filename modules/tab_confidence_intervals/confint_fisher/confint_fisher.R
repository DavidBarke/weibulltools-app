confint_fisher_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      bs4Dash::box(
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        title = "Fisher's Confidence Bounds",
        confint_fisher_fun_ui(
          id = ns("confint_fisher_fun")
        ),
        htmltools::br(),
        plot_conf_fun_ui(
          id = ns("plot_conf_fun")
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
      ) %>% add_connected_tabBox(
        id = ns("tabs_result")
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
          ),
          qf_incompatible_distributions_ui(
            id = ns("qf_incompatible_distributions")
          )
        )
      ) %>% add_connected_tabBox(
        id = ns("tabs_code")
      )
    )
  )
}

confint_fisher_server <- function(id,
                                  .values,
                                  ml_estimation_r,
                                  plot_prob_r
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      conf_fisher_return <- confint_fisher_fun_server(
        id = "confint_fisher_fun",
        .values = .values,
        ml_estimation_r = ml_estimation_r
      )

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

      plot_result_return <- plot_result_server(
        id = "plot_conf_result",
        .values = .values,
        p_obj_r = plot_conf_return$plot_conf_r
      )

      qf_incompatible_distributions_server(
        id = "qf_incompatible_distributions",
        .values = .values,
        error_message_r = plot_result_return$error_message_r,
        model_distribution_id = .values$ml_estimation_distribution_id,
        model_session = .values$ml_estimation_session
      )
    }
  )
}
