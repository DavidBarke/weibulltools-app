plot_result_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::uiOutput(
    outputId = ns("plot")
  )
}

plot_result_server <- function(id, .values, p_obj_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      is_plotly_plot_r <- shiny::reactive({
        "plotly" %in% class(p_obj_r())
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
        p_obj_r()
      })

      output$ggplot2 <- shiny::renderPlot({
        p_obj_r()
      })
    }
  )
}
