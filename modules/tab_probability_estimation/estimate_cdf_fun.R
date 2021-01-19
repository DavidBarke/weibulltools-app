estimate_cdf_fun_ui <- function(id, cdf_estimation_name) {
  ns <- shiny::NS(id)

  r_function(
    name = paste(cdf_estimation_name, " <- estimate_cdf"),
    r_function_arg(
      "x",
      htmltools::pre(
        "reliability_data(shock, x = distance, status = status)"
      )
    ),
    r_function_arg(
      "methods",
      preSelectInput(
        inputId = ns("methods"),
        label = NULL,
        choices = c("mr", "johnson", "kaplan", "nelson"),
        width = "100%"
      )
    ),
    r_function_arg(
      "options",
      shiny::uiOutput(
        outputId = ns("options")
      )
    )
  )
}

estimate_cdf_fun_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$options <- shiny::renderUI({
        if (shiny::req(input$methods[[1]]) == "mr") {
          htmltools::tagList(
            r_function_arg(
              "mr_method",
              width = 6,
              preSelectInput(
                inputId = ns("mr_method"),
                label = NULL,
                choices = c("benard", "invbeta"),
                width = "100%"
              )
            ),
            r_function_arg(
              "mr_ties.method",
              width = 6,
              preSelectInput(
                inputId = ns("mr_ties.method"),
                label = NULL,
                choices = c("max", "min", "average"),
                width = "100%"
              )
            )
          )
        } else {
          "list()"
        }
      })

      x_r <- shinymeta::metaReactive({
        reliability_data(shock, x = distance, status = status)
      }, varname = "rel_tbl")

      methods_r <- shinymeta::metaReactive({
        ..(input$methods)
      }, varname = "est_methods")

      options_r <- shinymeta::metaReactive2({
        if (shiny::req(input$methods[[1]]) == "mr") {
          shinymeta::metaExpr({
            list(
              mr_method = ..(input$mr_method),
              mr_ties.method = ..(input$mr_ties.method)
            )
          })
        } else {
          shinymeta::metaExpr({
            list()
          })
        }
      }, varname = "est_options")

      estimate_cdf_r <- shinymeta::metaReactive2({
        suppressMessages(shinymeta::metaExpr({
          estimate_cdf(
            x = ..(x_r()),
            methods = ..(methods_r()),
            options = ..(options_r())
          )
        }))
      }, varname = "cdf_tbl")

      return_list <- list(
        estimate_cdf_r = estimate_cdf_r
      )
    }
  )
}
