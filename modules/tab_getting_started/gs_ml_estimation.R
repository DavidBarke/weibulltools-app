gs_ml_estimation_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("distribution")
    ),
    purrr::map(distributions(include3 = TRUE), function(dist) {
      shiny::conditionalPanel(
        condition = glue::glue('input.distribution == "{dist}"'),
        r_code(
          'mle <- ml_estimation(',
          '  shock_tbl',
          glue::glue('  distribution = "{dist}"'),
          ')',
          '',
          'mle'
        ),
        ns = ns
      )
    }),
    list_result_ui(
      id = ns("ml_estimation")
    )
  )
}



gs_ml_estimation_server <- function(id,
                                    .values,
                                    reliability_data_r,
                                    distribution_r
) {
  shiny::moduleServer(
    id = id,
    function(input, output, session) {

      ns <- session$ns

      output$distribution <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("distribution"),
          label = "Distribution",
          choices = compatible_distributions(distribution_r())
        )
      })

      # This reactive is needed because the selectInput does not update when
      # the tabPanel is hidden
      safe_distribution_r <- shiny::reactive({
        if ((input$distribution %||% FALSE) %in% distribution_r()) {
          input$distribution
        } else {
          distribution_r()
        }
      })

      ml_estimation_r <- shiny::reactive({
        weibulltools::ml_estimation(
          reliability_data_r(),
          distribution = safe_distribution_r()
        )
      })

      list_result_server(
        id = "ml_estimation",
        .values = .values,
        obj_r = ml_estimation_r
      )

      return_list <- list(
        ml_estimation_r = ml_estimation_r
      )

      return(return_list)
    }
  )
}
