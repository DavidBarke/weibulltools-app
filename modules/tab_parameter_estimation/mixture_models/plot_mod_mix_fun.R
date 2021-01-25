plot_mod_mix_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "plot_mod",
    varname = "p_mod_mix",
    r_function_arg(
      "p_obj",
      htmltools::pre(
        shiny::uiOutput(
          outputId = ns("p_obj")
        )
      )
    ),
    r_function_arg(
      "x",
      htmltools::pre(
        shiny::uiOutput(
          outputId = ns("x")
        )
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

      output$p_obj <- shiny::renderUI({
        attr(plot_prob_r, "shinymetaVarname", exact = TRUE)
      })

      output$x <- shiny::renderUI({
        attr(model_r, "shinymetaVarname", exact = TRUE)
      })

      plot_mod_r <- shinymeta::metaReactive({
        plot_mod(
          p_obj = ..(plot_prob_r()),
          x = ..(model_r()),
          title_trace = ..(input$title_trace %||% "Fit")
        )
      }, varname = "p_mod_mix")

      return_list <- list(
        plot_mod_r = plot_mod_r
      )

      return(return_list)
    }
  )
}
