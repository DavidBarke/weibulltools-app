plot_conf_fun_ui <- function(id, plot_name, conf_name) {
  ns <- shiny::NS(id)

  r_function(
    name = "plot_conf",
    r_function_arg(
      "p_obj",
      plot_name
    ),
    r_function_arg(
      "x",
      conf_name
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

      plot_conf_r <- shinymeta::metaReactive({
        plot_conf(
          p_obj = ..(plot_prob_r()),
          x = ..(conf_r()),
          title_trace_mod = ..(shiny::req(input$title_trace_mod)),
          title_trace_conf = ..(shiny::req(input$title_trace_conf))
        )
      }, varname = "p_conf")

      return_list <- list(
        plot_conf_r = plot_conf_r
      )

      return(return_list)
    }
  )
}
