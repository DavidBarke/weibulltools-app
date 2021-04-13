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
    r_distribution_arg(
      inputId = ns("distribution")
    ),
    r_function_arg(
      "wts",
      shiny::uiOutput(
        outputId = ns("wts"),
        container = htmltools::pre
      )
    ),
    conf_level_ui(
      id = ns("conf_level")
    )
  )
}

ml_estimation_fun_server <- function(id, .values, reliability_data_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      rd_varname <- attr(reliability_data_r, "shinymetaVarname", exact = TRUE)

      output$placeholder <- shiny::renderUI({
        glue::glue(
          '
          x = {x},
          distribution = "{distribution}",
          conf_level = {conf_level}
          ',
          x = rd_varname,
          distribution = input$distribution,
          conf_level = conf_level_return$conf_level_r()
        )
      })

      shiny::outputOptions(
        output,
        "placeholder",
        suspendWhenHidden = FALSE
      )

      output$x <- shiny::renderUI({
        varname_link(
          tabName = "reliability_data",
          varname = rd_varname
        )
      })

      output$wts <- shiny::renderUI({
        htmltools::tagList(
          "rep(1, nrow(",
          varname_link(
            tabName = "reliability_data",
            varname = rd_varname
          ),
          "))"
        )
      })

      distribution_r <- shiny::reactive({
        input$distribution %||% "weibull"
      })

      # For qf_incompatible_distribution
      .values$ml_estimation_distribution_id <- "distribution"
      .values$ml_estimation_session <- session

      ml_estimation_r <- shinymeta::metaReactive({
        ml_estimation(
          x = ..(reliability_data_r()),
          distribution = ..(distribution_r()),
          conf_level = ..(conf_level_return$conf_level_r())
        )
      }, varname = "mle")

      conf_level_return <- conf_level_server(
        id = "conf_level",
        .values = .values
      )

      return_list <- list(
        ml_estimation_r = ml_estimation_r
      )

      return(return_list)
    }
  )
}
