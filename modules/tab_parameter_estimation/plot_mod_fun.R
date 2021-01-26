plot_mod_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "plot_mod",
    varname = r_function_varname("p_mod"),
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

plot_mod_fun_server <- function(id, .values, model_r, plot_prob_r) {
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

      model_varname <- attr(model_r, "shinymetaVarname", exact = TRUE)

      output$x <- shiny::renderUI({
        varname_link_ui(
          id = ns("varname_link_model"),
          varname = model_varname
        )
      })

      varname_link_server(
        id = "varname_link_model",
        .values = .values,
        tabName = NULL,
        varname = model_varname
      )

      plot_mod_r <- shinymeta::metaReactive({
        plot_mod(
          p_obj = ..(plot_prob_r()),
          x = ..(model_r()),
          title_trace = ..(input$title_trace %||% "Fit")
        )
      }, varname = "p_mod")

      return_list <- list(
        plot_mod_r = plot_mod_r
      )

      return(return_list)
    }
  )
}
