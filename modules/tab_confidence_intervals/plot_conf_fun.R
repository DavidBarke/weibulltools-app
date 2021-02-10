plot_conf_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "plot_conf",
    varname = r_function_varname("p_conf"),
    placeholder = shiny::uiOutput(
      outputId = ns("placeholder"),
      container = htmltools::pre
    ),
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

      conf_varname <- attr(conf_r, "shinymetaVarname", exact = TRUE)
      plot_prob_varname <- attr(plot_prob_r, "shinymetaVarname", exact = TRUE)

      title_trace_mod_r <- shiny::reactive({
        input$title_trace_mod %||% "Fit"
      })

      title_trace_conf_r <- shiny::reactive({
        input$title_trace_conf %||% "Confidence Limit"
      })

      output$placeholder <- shiny::renderUI({
        glue::glue(
          '
          p_obj = {p_obj},
          x = {x},
          title_trace_mod = "{title_trace_mod}",
          title_trace_conf = "{title_trace_conf}"
          ',
          p_obj = plot_prob_varname,
          x = conf_varname,
          title_trace_mod = title_trace_mod_r(),
          title_trace_conf = title_trace_conf_r()
        )
      })

      shiny::outputOptions(
        output,
        "placeholder",
        suspendWhenHidden = FALSE
      )

      output$p_obj <- shiny::renderUI({
        varname_link(
          tabName = "probability_estimation",
          varname = plot_prob_varname
        )
      })

      output$x <- shiny::renderUI({
        varname_link(
          tabName = NULL,
          varname = conf_varname
        )
      })

      plot_conf_r <- shinymeta::metaReactive({
        plot_conf(
          p_obj = ..(plot_prob_r()),
          x = ..(conf_r()),
          title_trace_mod = ..(replace_comma(title_trace_mod_r())),
          title_trace_conf = ..(replace_comma(title_trace_conf_r()))
        )
      }, varname = "p_conf")

      return_list <- list(
        plot_conf_r = plot_conf_r
      )

      return(return_list)
    }
  )
}
