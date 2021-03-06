rank_regression_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      bs4Dash::box(
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        title = "Rank Regression",
        rank_regression_fun_ui(
          id = ns("rank_regression_fun")
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
          id = ns("rank_regression_code"),
          title = "rank_regression"
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
          title = "rank_regression",
          list_result_ui(
            id = ns("rank_regression_result")
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

rank_regression_server <- function(id,
                                   .values,
                                   estimate_cdf_r,
                                   plot_prob_r
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      rr_return <- rank_regression_fun_server(
        id = "rank_regression_fun",
        .values = .values,
        estimate_cdf_r = estimate_cdf_r
      )

      plot_mod_return <- plot_mod_fun_server(
        id = "plot_mod_fun",
        .values = .values,
        model_r = rr_return$rank_regression_r,
        plot_prob_r = plot_prob_r
      )

      code_tab_server(
        id = "rank_regression_code",
        .values = .values,
        rr_return$rank_regression_r
      )

      code_tab_server(
        id = "plot_mod_code",
        .values = .values,
        obj_r = plot_mod_return$plot_mod_r
      )

      list_result_server(
        id = "rank_regression_result",
        .values = .values,
        obj_r = rr_return$rank_regression_r
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
        model_distribution_id = .values$rank_regression_distribution_id,
        model_session = .values$rank_regression_session
      )

      return_list <- list(
        rank_regression_r = rr_return$rank_regression_r
      )

      return(return_list)
    }
  )
}
