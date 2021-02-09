ml_estimation_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "ml_estimation",
    varname = ref_dropdown(
      varname = r_function_varname("mle"),
      ref_tbl = tibble::tibble(
        label = c("plot_mod", "confint_fisher"),
        reference = c("plot_mod", "confint_fisher"),
        tabName = c("ml_estimation", "confint_fisher")
      )
    ),
    r_function_arg(
      "x",
      shiny::uiOutput(
        outputId = ns("x"),
        container = htmltools::pre
      )
    ),
    r_distribution_arg(
      inputId = ns("distribution")
    ),
    r_function_arg(
      "wts",
      htmltools::pre("rep(1, nrow(x))")
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

      rd_varname <- attr(reliability_data_r, "shinymetaVarname", exact = TRUE)

      output$x <- shiny::renderUI({
        varname_link(
          tabName = "reliability_data",
          varname = rd_varname
        )
      })

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
