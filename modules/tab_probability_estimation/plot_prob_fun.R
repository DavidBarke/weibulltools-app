plot_prob_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "plot_prob",
    varname = ref_dropdown(
      varname = r_function_varname("p_prob"),
      ref_tbl = tibble::tibble(
        label = c(
          "ML Estimation: plot_mod", "Rank Regression: plot_mod",
          "Beta Binomial CI: plot_conf", "Fisher CI: plot_conf"
        ),
        reference = c(
          "plot_mod", "plot_mod",
          "plot_conf", "plot_conf"
        ),
        tabName = c(
          "ml_estimation", "rank_regression",
          "confint_betabinom", "confint_fisher"
        )
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
      inputId = ns("distribution"),
      include_thres = FALSE
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
    r_function_arg(
      name = "plot_method",
      htmltools::pre("plotly")
    )
  )
}

plot_prob_fun_server <- function(id, .values, estimate_cdf_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cdf_varname <- attr(estimate_cdf_r, "shinymetaVarname", exact = TRUE)

      distribution_r <- shiny::reactive({
        input$distribution %||% "weibull"
      })

      # For fix_plot_mod quick fix
      .values$plot_prob_distribution_id <- "distribution"
      .values$plot_prob_session <- session

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
        "plotly"
      })

      output$placeholder <- shiny::renderUI({
        glue::glue(
          '
          x = {x},
          distribution = "{distribution}",
          title_main = "{title_main}",
          title_x = "{title_x}",
          title_y = "{title_y}",
          title_trace = "{title_trace}",
          plot_method = "{plot_method}"
          ',
          x = cdf_varname,
          distribution = distribution_r(),
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
        varname_link(
          tabName = NULL,
          varname = cdf_varname
        )
      })

      plot_prob_r <- shinymeta::metaReactive({
        plot_prob(
          x = ..(estimate_cdf_r()),
          distribution = ..(distribution_r()),
          title_main = ..(replace_comma(title_main_r())),
          title_x = ..(replace_comma(title_x_r())),
          title_y = ..(replace_comma(title_y_r())),
          title_trace = ..(replace_comma(title_trace_r())),
          plot_method = ..(plot_method_r())
        )
      }, varname = "p_prob")

      return_list <- list(
        plot_prob_r = plot_prob_r
      )

      return(return_list)
    }
  )
}
