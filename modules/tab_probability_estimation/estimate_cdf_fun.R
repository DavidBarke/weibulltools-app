estimate_cdf_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    id = ns("function"),
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

estimate_cdf_fun_server <- function(id, .values, cdf_estimation_name) {
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

      methods_r <- shiny::reactive({
        shiny::req(input$methods)
      })

      options_r <- shiny::reactive({
        if (shiny::req(input$methods[[1]]) == "mr") {
          list(
            mr_method = shiny::req(input$mr_method),
            mr_ties.method = shiny::req(input$mr_ties.method)
          )
        } else {
          list()
        }
      })

      estimate_cdf_r <- shinymeta::metaReactive2({
        # Force evaluation of options_r before it is passed to estimate_cdf
        options_r()

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
