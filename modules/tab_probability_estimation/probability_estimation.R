probability_estimation_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      bs4Dash::box(
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        title = "Non-Parametric Failure Probabilities",
        htmltools::p(
          "Functions for estimation and visualization of failure probabilities"
        ),
        r_function(
          name = "estimate_cdf",
          r_function_arg(
            "x",

          ),
          r_function_arg(
            "methods",
            shiny::selectInput(
              inputId = ns("select_methods"),
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
      )
    )
  )
}

probability_estimation_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$options <- shiny::renderUI({
        if (shiny::req(input$select_methods[[1]]) == "mr") {
          htmltools::tagList(
            r_function_arg(
              "mr_method",
              width = 6,
              shiny::selectInput(
                inputId = ns("mr_method"),
                label = NULL,
                choices = c("benard", "invbeta"),
                width = "100%"
              )
            ),
            r_function_arg(
              "mr_ties.method",
              width = 6,
              shiny::selectInput(
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
    }
  )
}
