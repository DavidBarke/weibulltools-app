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
        htmltools::br(),
        plot_mod_fun_ui(
          id = ns("plot_mod_fun")
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
          id = ns("ml_estimation_code"),
          title = "ml_estimation"
        ),
        code_tab_ui(
          id = ns("plot_mod_code"),
          title = "plot_mod"
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
          title = "ml_estimation",
          list_result_ui(
            id = ns("ml_estimation_result")
          )
        ),
        shiny::tabPanel(
          title = "plot_mod",
          plot_result_ui(
            id = ns("plot_mod_result")
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

ml_estimation_server <- function(id,
                                 .values,
                                 reliability_data_r,
                                 plot_prob_r
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      ml_estimation_return <- ml_estimation_fun_server(
        id = "ml_estimation_fun",
        .values = .values,
        reliability_data_r = reliability_data_r
      )

      plot_mod_return <- plot_mod_fun_server(
        id = "plot_mod_fun",
        .values = .values,
        model_r = ml_estimation_return$ml_estimation_r,
        plot_prob_r = plot_prob_r
      )

      code_tab_server(
        id = "ml_estimation_code",
        .values = .values,
        ml_estimation_return$ml_estimation_r
      )

      code_tab_server(
        id = "plot_mod_code",
        .values = .values,
        obj_r = plot_mod_return$plot_mod_r
      )

      list_result_server(
        id = "ml_estimation_result",
        .values = .values,
        obj_r = ml_estimation_return$ml_estimation_r
      )

      plot_result_return <- plot_result_server(
        id = "plot_mod_result",
        .values = .values,
        p_obj_r = plot_mod_return$plot_mod_r
      )

      qf_incompatible_distributions_server(
        id = "qf_incompatible_distributions",
        .values = .values,
        error_message_r = plot_result_return$error_message_r,
        model_distribution_id = .values$ml_estimation_distribution_id,
        model_session = .values$ml_estimation_session
      )

      return_list <- list(
        ml_estimation_r = ml_estimation_return$ml_estimation_r
      )

      return(return_list)
    }
  )
}
