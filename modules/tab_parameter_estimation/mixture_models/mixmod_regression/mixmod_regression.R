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
          id = ns("plot_prob_mix_fun")
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

      # shiny::observeEvent(input$tabs_code, {
      #   shiny::updateTabsetPanel(
      #     inputId = "tabs_result",
      #     selected = input$tabs_code
      #   )
      # })
      #
      # shiny::observeEvent(input$tabs_result, {
      #   shiny::updateTabsetPanel(
      #     inputId = "tabs_code",
      #     selected = input$tabs_result
      #   )
      # })

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

      list_result_server(
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

      return_list <- list(
        mixmod_regression_r = mixmod_regression_return$mixmod_regression_r
      )

      return(return_list)
    }
  )
}
