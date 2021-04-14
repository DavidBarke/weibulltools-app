mixmod_regression_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      bs4Dash::box(
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        title = "Segmented Regression",
        mixmod_regression_fun_ui(
          id = ns("mixmod_regression_fun")
        ),
        htmltools::br(),
        plot_prob_mix_fun_ui(
          id = ns("plot_prob_mix_fun"),
          ref_tabName = "mixmod_regression"
        ),
        htmltools::br(),
        plot_mod_mix_fun_ui(
          id = ns("plot_mod_mix_fun")
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
          id = ns("mixmod_regression_code"),
          title = "mixmod_regression"
        ),
        code_tab_ui(
          id = ns("plot_prob_mix_code"),
          title = "plot_prob"
        ),
        code_tab_ui(
          id = ns("plot_mod_mix_code"),
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
          title = "mixmod_regression",
          list_result_ui(
            id = ns("mixmod_regression_result")
          )
        ),
        shiny::tabPanel(
          title = "plot_prob",
          plot_result_ui(
            id = ns("plot_prob_mix_result")
          )
        ),
        shiny::tabPanel(
          title = "plot_mod",
          plot_result_ui(
            id = ns("plot_mod_mix_result")
          )
        )
      ) %>% append_ui(
        qf_mixmod_ui(
          id = ns("qf_mixmod")
        )
      ) %>% add_connected_tabBox(
        id = ns("tabs_code")
      )
    )
  )
}

mixmod_regression_server <- function(id,
                                     .values,
                                     estimate_cdf_r
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      mixmod_regression_return <- mixmod_regression_fun_server(
        id = "mixmod_regression_fun",
        .values = .values,
        estimate_cdf_r = estimate_cdf_r
      )

      plot_prob_mix_return <- plot_prob_mix_fun_server(
        id = "plot_prob_mix_fun",
        .values = .values,
        model_r = mixmod_regression_return$mixmod_regression_r
      )

      plot_mod_mix_return <- plot_mod_mix_fun_server(
        id = "plot_mod_mix_fun",
        .values = .values,
        model_r = mixmod_regression_return$mixmod_regression_r,
        plot_prob_r = plot_prob_mix_return$plot_prob_mix_r
      )

      code_tab_server(
        id = "mixmod_regression_code",
        .values = .values,
        obj_r = mixmod_regression_return$mixmod_regression_r
      )

      code_tab_server(
        id = "plot_prob_mix_code",
        .values = .values,
        obj_r = plot_prob_mix_return$plot_prob_mix_r
      )

      code_tab_server(
        id = "plot_mod_mix_code",
        .values = .values,
        obj_r = plot_mod_mix_return$plot_mod_r
      )

      list_result_return <- list_result_server(
        id = "mixmod_regression_result",
        .values = .values,
        obj_r = mixmod_regression_return$mixmod_regression_r,
        dynamic = TRUE
      )

      plot_result_server(
        id = "plot_prob_mix_result",
        .values = .values,
        p_obj_r = plot_prob_mix_return$plot_prob_mix_r
      )

      plot_result_server(
        id = "plot_mod_mix_result",
        .values = .values,
        p_obj_r = plot_mod_mix_return$plot_mod_r
      )

      # qf_mixmod is shared between results and both plots as error
      # depends on the result exclusively. Therefore only the error message
      # of list_result is taken into account
      qf_mixmod_server(
        id = "qf_mixmod",
        .values = .values,
        error_message_r = list_result_return$error_message_r,
        k_id = .values$mixmod_regression_k_id,
        k_session = .values$mixmod_regression_session
      )

      return_list <- list(
        mixmod_regression_r = mixmod_regression_return$mixmod_regression_r
      )

      return(return_list)
    }
  )
}
