ml_estimation_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "ml_estimation",
    varname = "mle",
    r_function_arg(
      "x",
      shiny::uiOutput(
        outputId = ns("x")
      )
    ),
    r_distribution_arg(
      inputId = ns("distribution")
    ),
    r_function_arg(
      "wts",
      "rep(1, nrow(x))"
    ),
    r_conf_level_arg(
      inputId = ns("conf_level")
    )
  )
}

ml_estimation_fun_server <- function(id, .values, reliability_data_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$x <- shiny::renderUI({
        varname_link_ui(
          id = ns("varname_link_reliability_data"),
          name = attr(reliability_data_r, "shinymetaVarname", exact = TRUE)
        )
      })

      varname_link_server(
        id = "varname_link_reliability_data",
        .values = .values,
        tabName = "reliability_data"
      )

      ml_estimation_r <- shinymeta::metaReactive({
        ml_estimation(
          x = ..(reliability_data_r()),
          distribution = ..(shiny::req(input$distribution)),
          conf_level = ..(shiny::req(input$conf_level))
        )
      }, varname = "mle")

      return_list <- list(
        ml_estimation_r = ml_estimation_r
      )

      return(return_list)
    }
  )
}
