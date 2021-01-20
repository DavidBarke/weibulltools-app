code_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
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

code_server <- function(id, .values, obj_r) {
  shiny::moduleServer(
    id = id,
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

      formatted_code_r <- shiny::reactive({
        x <- styler::style_text(stringr::str_replace_all(code_r(), ",", "\n,"))
        stringr::str_replace_all(x, "@@", ",")
      })

      output$code <- shiny::renderUI({
        shinyAce::aceEditor(
          outputId = ns("code_ace"),
          value = formatted_code_r(),
          mode = "r",
          readOnly = TRUE,
          height = "250px",
          fontSize = 14
        )
      })

      shiny::observeEvent(input$copy_to_clipboard, {
        clipr::write_clip(formatted_code_r(), allow_non_interactive = TRUE)
      })
    }
  )
}

replace_comma <- function(x) {
  shiny::req(x)
  stringr::str_replace_all(x, ",", "@@")
}

