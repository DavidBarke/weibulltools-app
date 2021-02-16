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
        inherits(p_obj_r(), "plotly")
      })

      output$plot <- shiny::renderUI({
        if (error_display_return$error_r()) return(NULL)

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
        error_display_return$obj_r()
      })

      output$ggplot2 <- shiny::renderPlot({
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
