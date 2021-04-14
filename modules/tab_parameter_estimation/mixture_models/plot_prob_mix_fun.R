plot_prob_mix_fun_ui <- function(id, ref_tabName) {
  ns <- shiny::NS(id)

  r_function(
    name = "plot_prob",
    varname = ref_dropdown(
      varname = r_function_varname("p_prob_mix"),
      ref_tbl = tibble::tibble(
        label = "plot_mod",
        reference = "plot_mod",
        tabName = ref_tabName
      )
    ),
    placeholder = shiny::uiOutput(
      outputId = ns("placeholder"),
      container = htmltools::pre
    ),
    r_function_arg(
      "x",
      shiny::uiOutput(
        outputId = ns("x"),
        container = htmltools::pre
      )
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

plot_prob_mix_fun_server <- function(id, .values, model_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      model_varname <- attr(model_r, "shinymetaVarname", exact = TRUE)

      title_main_r <- shiny::reactive({
        input$title_main %||% "Probability Plot"
      })

      title_x_r <- shiny::reactive({
        input$title_x %||% "Characteristic"
      })

      title_y_r <- shiny::reactive({
        input$title_y %||% "Unreliability"
      })

      title_trace_r <- shiny::reactive({
        input$title_trace %||% "Sample"
      })

      plot_method_r <- shiny::reactive({
        input$plot_method %||% "plotly"
      })

      output$placeholder <- shiny::renderUI({
        glue::glue(
          '
          x = {x},
          title_main = "{title_main}",
          title_x = "{title_x}",
          title_y = "{title_y}",
          title_trace = "{title_trace}",
          plot_method = "{plot_method}"
          ',
          x = model_varname,
          title_main = title_main_r(),
          title_x = title_x_r(),
          title_y = title_y_r(),
          title_trace = title_trace_r(),
          plot_method = plot_method_r()
        )
      })

      shiny::outputOptions(
        output,
        "placeholder",
        suspendWhenHidden = FALSE
      )

      output$x <- shiny::renderUI({
        model_varname
      })

      plot_prob_mix_r <- shinymeta::metaReactive({
        plot_prob(
          x = ..(model_r()),
          title_main = ..(replace_comma(title_main_r())),
          title_x = ..(replace_comma(title_x_r())),
          title_y = ..(replace_comma(title_y_r())),
          title_trace = ..(replace_comma(title_trace_r())),
          plot_method = ..(plot_method_r())
        )
      }, varname = "p_prob_mix")

      return_list <- list(
        plot_prob_mix_r = plot_prob_mix_r
      )

      return(return_list)
    }
  )
}
