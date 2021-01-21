plot_mod_fun_ui <- function(id, plot_name, model_name) {
  ns <- shiny::NS(id)

  r_function(
    name = "plot_mod",
    r_function_arg(
      "p_obj",
      plot_name
    ),
    r_function_arg(
      "x",
      model_name
    ),
    r_text_arg(
      name = "title_trace",
      inputId = ns("title_trace"),
      value = "Fit"
    )
  )
}

plot_mod_fun_server <- function(id, .values, model_r, plot_prob_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      plot_mod_r <- shinymeta::metaReactive({
        plot_mod(
          p_obj = ..(plot_prob_r()),
          x = ..(model_r()),
          title_trace = ..(shiny::req(input$title_trace))
        )
      }, varname = "p_mod")

      return_list <- list(
        plot_mod_r = plot_mod_r
      )

      return(return_list)
    }
  )
}
