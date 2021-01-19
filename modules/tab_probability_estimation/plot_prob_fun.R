plot_prob_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    id = ns("function"),
    name = "plot_prob",
    r_function_arg(
      "x",
      htmltools::pre(cdf_estimation_name)
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

plot_prob_fun_server <- function(id, .values, cdf_estimation_name, estimate_cdf_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      return_list <- list(
        plot_prob_r = estimate_cdf_r
      )

      return(return_list)
    }
  )
}
