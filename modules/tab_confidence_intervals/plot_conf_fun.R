plot_conf_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "plot_conf",
    r_function_arg(
      "p_obj",
      shiny::uiOutput(
        outputId = ns("p_obj")
      )
    ),
    r_function_arg(
      "x",
      shiny::uiOutput(
        outputId = ns("x")
      )
    ),
    r_text_arg(
      name = "title_trace_mod",
      inputId = ns("title_trace_mod"),
      value = "Fit"
    ),
    r_text_arg(
      name = "title_trace_conf",
      inputId = ns("title_trace_conf"),
      value = "Confidence Limit"
    )
  )
}

plot_conf_fun_server <- function(id, .values, conf_r, plot_prob_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$p_obj <- shiny::renderUI({
        varname_link_ui(
          id = ns("varname_link_probability_estimation"),
          name = attr(plot_prob_r, "shinymetaVarname", exact = TRUE)
        )
      })

      varname_link_server(
        id = "varname_link_probability_estimation",
        .values = .values,
        tabName = "probability_estimation"
      )

      output$x <- shiny::renderUI({
        attr(conf_r, "shinymetaVarname", exact = TRUE)
      })

      plot_conf_r <- shinymeta::metaReactive({
        plot_conf(
          p_obj = ..(plot_prob_r()),
          x = ..(conf_r()),
          title_trace_mod = ..(input$title_trace_mod %||% "Fit"),
          title_trace_conf = ..(input$title_trace_conf %||% "Confidence Limit")
        )
      }, varname = "p_conf")

      return_list <- list(
        plot_conf_r = plot_conf_r
      )

      return(return_list)
    }
  )
}
