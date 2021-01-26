estimate_cdf_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "estimate_cdf",
    varname = "cdf_tbl",
    r_function_arg(
      "x",
      shiny::uiOutput(
        outputId = ns("x"),
        container = htmltools::pre
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

estimate_cdf_fun_server <- function(id,
                                    .values,
                                    reliability_data_r
) {
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

      output$options <- shiny::renderUI({
        if ("mr" %in% (input$methods %||% "mr")) {
          htmltools::tagList(
            r_function_arg(
              "mr_method",
              width = 6,
              preSelectInput(
                inputId = ns("mr_method"),
                label = NULL,
                choices = c("benard", "invbeta"),
                width = "100%"
              ),
              standalone = TRUE
            ),
            r_function_arg(
              "mr_ties.method",
              width = 6,
              preSelectInput(
                inputId = ns("mr_ties.method"),
                label = NULL,
                choices = c("max", "min", "average"),
                width = "100%"
              ),
              standalone = TRUE
            )
          )
        } else {
          htmltools::pre("list()")
        }
      })

      methods_r <- shiny::reactive({
        input$methods %||% "mr"
      })

      options_r <- shiny::reactive({
        if ("mr" %in% (input$methods %||% "mr")) {
          list(
            mr_method = input$mr_method %||% "benard",
            mr_ties.method = input$mr_ties.method %||% "max"
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
            x = ..(reliability_data_r()),
            methods = ..(methods_r()),
            options = ..(options_r())
          )
        }))
      }, varname = "cdf_tbl")

      return_list <- list(
        estimate_cdf_r = estimate_cdf_r
      )

      return(return_list)
    }
  )
}
