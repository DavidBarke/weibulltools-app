ml_estimation_ui <- function(id) {
  ns <- shiny::NS(id)

  model_name <- "mle"

  shiny::fluidRow(
    shiny::column(
      width = 6,
      bs4Dash::box(
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        title = "ML Estimation",
        ml_estimation_fun_ui(
          id = ns("ml_estimation_fun"),
          model_name = model_name
        ),
        htmltools::br(),
        plot_mod_fun_ui(
          id = ns("plot_mod_fun"),
          plot_name = "p_prob",
          model_name = model_name
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
          )
        )
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

      plot_result_server(
        id = "plot_mod_result",
        .values = .values,
        p_obj_r = plot_mod_return$plot_mod_r
      )

      return_list <- list(
        ml_estimation_r = ml_estimation_return$ml_estimation_r
      )

      return(return_list)
    }
  )
}
