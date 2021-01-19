code_box_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    title = "Code",
    shiny::verbatimTextOutput(
      outputId = ns("code")
    )
  )
}

code_box_server <- function(id, .values, obj_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$code <- shiny::renderPrint({
        shinymeta::expandChain(
          .values$code_header,
          obj_r(),
          .expansionContext = shinymeta::newExpansionContext(ns = FALSE)
        )
      })
    }
  )
}
