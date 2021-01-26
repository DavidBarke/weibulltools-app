plot_conf_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "plot_conf",
    varname = r_function_varname("p_conf"),
    r_function_arg(
      "p_obj",
      shiny::uiOutput(
        outputId = ns("p_obj"),
        container = htmltools::pre
      )
    ),
    r_function_arg(
      "x",
      shiny::uiOutput(
        outputId = ns("x"),
        container = htmltools::pre
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

      plot_prob_varname <- attr(plot_prob_r, "shinymetaVarname", exact = TRUE)

      output$p_obj <- shiny::renderUI({
        varname_link_ui(
          id = ns("varname_link_probability_estimation"),
          varname = plot_prob_varname
        )
      })

      varname_link_server(
        id = "varname_link_probability_estimation",
        .values = .values,
        tabName = "probability_estimation",
        varname = plot_prob_varname
      )

      conf_varname <- attr(conf_r, "shinymetaVarname", exact = TRUE)

      output$x <- shiny::renderUI({
        varname_link_ui(
          id = ns("varname_link_conf"),
          varname = conf_varname
        )
      })

      varname_link_server(
        id = "varname_link_conf",
        .values = .values,
        tabName = NULL,
        varname = conf_varname
      )

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
