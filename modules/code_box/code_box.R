code_box_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    title = "Code",
    shiny::uiOutput(
      outputId = ns("code")
    ),
    shiny::actionButton(
      inputId = ns("copy_to_clipboard"),
      label = NULL,
      icon = shiny::icon("clipboard"),
      `data-toggle` = "popover-click",
      `data-content` = "Copied to clipboard!"
    )
  )
}

code_box_server <- function(id, .values, obj_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      code_r <- shiny::reactive({
        shinymeta::formatCode(
          shinymeta::expandChain(
            .values$code_header,
            obj_r(),
            .expansionContext = shinymeta::newExpansionContext(ns = FALSE)
          )
        )
      })

      output$code <- shiny::renderUI({
        shinyAce::aceEditor(
          outputId = ns("code_ace"),
          value = code_r(),
          mode = "r",
          readOnly = TRUE,
          height = "250px",
          fontSize = 14
        )
      })

      shiny::observeEvent(input$copy_to_clipboard, {
        clipr::write_clip(code_r())
      })
    }
  )
}
