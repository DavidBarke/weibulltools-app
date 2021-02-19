plot_result_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    error_display_ui(
      id = ns("error_display")
    ),
    shiny::uiOutput(
      outputId = ns("plot")
    ) %>% shinycssloaders::withSpinner()
  )
}

plot_result_server <- function(id, .values, p_obj_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      is_plotly_plot_r <- shiny::reactive({
        if (inherits(error_display_return$obj_r(), "plotly")) {
          TRUE
        } else if (inherits(error_display_return$obj_r(), "ggplot")) {
          FALSE
        } else {
          shiny::req(NULL)
        }
      })

      output$plot <- shiny::renderUI({
        if (is_plotly_plot_r()) {
          plotly::plotlyOutput(
            outputId = ns("plotly")
          )
        } else {
          shiny::plotOutput(
            outputId = ns("ggplot2")
          )
        }
      })

      output$plotly <- plotly::renderPlotly({
        shiny::req(is_plotly_plot_r())
        error_display_return$obj_r()
      })

      output$ggplot2 <- shiny::renderPlot({
        shiny::req(!is_plotly_plot_r())
        error_display_return$obj_r()
      })

      error_display_return <- error_display_server(
        id = "error_display",
        .values = .values,
        obj_r = p_obj_r
      )
    }
  )
}
