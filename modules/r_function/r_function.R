r_function_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 12,
      shiny::uiOutput(
        outputId = ns("ui"),
        class = "r-function"
      )
    )
  )
}

r_function_server <- function(id, .values, name, ...) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      toggle_rv <- shiny::reactiveVal(TRUE)

      output$ui <- shiny::renderUI({
        if (toggle_rv()) {
          htmltools::tagList(
            htmltools::div(
              class = "flex-container",
              htmltools::pre(paste0(name, "(")),
              shiny::actionButton(
                inputId = ns("toggle"),
                label = NULL,
                icon = shiny::icon("chevron-down")
              )
            ),
            htmltools::div(
              class = "r-function-body",
              ...
            ),
            htmltools::pre(")")
          )
        } else {
          htmltools::div(
            class = "flex-container",
            htmltools::pre(paste0(name, "(...)")),
            shiny::actionButton(
              inputId = ns("toggle"),
              label = NULL,
              icon = shiny::icon("chevron-up")
            )
          )
        }
      })

      shiny::observeEvent(input$toggle, {
        toggle_rv(!toggle_rv())
      })
    }
  )
}
