plot_mod_mix_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "plot_mod",
    varname = r_function_varname("p_mod_mix"),
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
      name = "title_trace",
      inputId = ns("title_trace"),
      value = "Fit"
    )
  )
}

plot_mod_mix_fun_server <- function(id, .values, model_r, plot_prob_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      model_varname <- attr(model_r, "shinymetaVarname", exact = TRUE)
      plot_prob_varname <- attr(plot_prob_r, "shinymetaVarname", exact = TRUE)

      title_trace_r <- shiny::reactive({
        input$title_trace %||% "Fit"
      })

      output$placeholder <- shiny::renderUI({
        glue::glue(
          '
          p_obj = {p_obj},
          x = {x},
          title_trace = "{title_trace}"
          ',
          p_obj = plot_prob_varname,
          x = model_varname,
          title_trace = title_trace_r()
        )
      })

      shiny::outputOptions(
        output,
        "placeholder",
        suspendWhenHidden = FALSE
      )

      output$p_obj <- shiny::renderUI({
        plot_prob_varname
      })

      output$x <- shiny::renderUI({
        model_varname
      })

      plot_mod_r <- shinymeta::metaReactive({
        plot_mod(
          p_obj = ..(plot_prob_r()),
          x = ..(model_r()),
          title_trace = ..(replace_comma(title_trace_r()))
        )
      }, varname = "p_mod_mix")

      return_list <- list(
        plot_mod_r = plot_mod_r
      )

      return(return_list)
    }
  )
}
