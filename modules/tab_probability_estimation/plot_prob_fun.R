plot_prob_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "plot_prob",
    varname = "p_prob",
    r_function_arg(
      "x",
      shiny::uiOutput(
        outputId = ns("x"),
        container = htmltools::pre
      )
    ),
    r_distribution_arg(
      inputId = ns("distribution"),
      include3 = FALSE
    ),
    r_text_arg(
      name = "title_main",
      inputId = ns("title_main"),
      value = "Probability Plot"
    ),
    r_text_arg(
      name = "title_x",
      inputId = ns("title_x"),
      value = "Characteristic"
    ),
    r_text_arg(
      name = "title_y",
      inputId = ns("title_y"),
      value = "Unreliability"
    ),
    r_text_arg(
      name = "title_trace",
      inputId = ns("title_trace"),
      value = "Sample"
    ),
    r_plot_method_arg(
      inputId = ns("plot_method")
    )
  )
}

plot_prob_fun_server <- function(id, .values, estimate_cdf_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$x <- shiny::renderUI({
        attr(estimate_cdf_r, "shinymetaVarname", exact = TRUE)
      })

      plot_prob_r <- shinymeta::metaReactive({
        plot_prob(
          x = ..(estimate_cdf_r()),
          distribution = ..(replace_comma(input$distribution %||% "weibull")),
          title_main = ..(replace_comma(input$title_main %||% "Probability Plot")),
          title_x = ..(replace_comma(input$title_x %||% "Characteristic")),
          title_y = ..(replace_comma(input$title_y %||% "Unreliability")),
          title_trace = ..(replace_comma(input$title_trace %||% "Sample")),
          plot_method = ..(replace_comma(input$plot_method %||% "plotly"))
        )
      }, varname = "p_prob")

      return_list <- list(
        plot_prob_r = plot_prob_r
      )

      return(return_list)
    }
  )
}
