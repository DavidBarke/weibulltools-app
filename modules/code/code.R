code_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("code")
    ),
    shiny::uiOutput(
      outputId = ns("clip_btn")
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
          height = "300px",
          fontSize = 14
        )
      })

      output$clip_btn <- shiny::renderUI({
        ui <- rclipboard::rclipButton(
          inputId = ns("clip"),
          label = NULL,
          icon = shiny::icon("clipboard"),
          clipText = paste(formatted_code_r(), collapse = "\n")
        )

        ui[[1]] <- htmltools::tagAppendAttributes(
          ui[[1]],
          `data-toggle` = "popover-click",
          `data-content` = "Copied to clipboard!"
        )

        ui <- htmltools::tagList(
          ui,
          htmltools::tags$script(
            "bindPopoverClick();"
          )
        )
      })
    }
  )
}

replace_comma <- function(x) {
  shiny::req(x)
  stringr::str_replace_all(x, ",", "@@")
}

