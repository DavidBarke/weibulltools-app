plot_mod_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "plot_mod",
    r_function_arg(
      "p_obj",
      "p_prob"
    ),
    r_function_arg(
      "x",
      "rr"
    ),
    r_text_arg(
      name = "title_trace",
      inputId = ns("title_trace"),
      value = "Fit"
    )
  )
}

plot_mod_fun_server <- function(id, .values, model_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
    }
  )
}
