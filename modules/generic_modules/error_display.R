error_display_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::textOutput(
      outputId = ns("error")
    ),
    shiny::textOutput(
      outputId = ns("warning")
    )
  )
}

error_display_server <- function(id, .values, obj_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      error_message_rv <- shiny::reactiveVal(NULL)
      warning_message_rv <- shiny::reactiveVal(character())

      output$error <- shiny::renderText({
        error_message_rv()
      })

      output$warning <- shiny::renderText({
        paste(warning_message_rv(), collapse = "\n")
      })

      shiny::observeEvent(suppressWarnings(try(obj_r(), silent = TRUE)), {
        tryCatch(
          withCallingHandlers(
            {
              # Clean up error and warning messages
              error_message_rv(NULL)
              warning_message_rv(character())
              # Evaluate obj_r
              force(obj_r())
            },
            warning = function(e) {
              warning_message_rv(
                c(
                  shiny::isolate(warning_message_rv()),
                  e$message
                )
              )
              invokeRestart("muffleWarning")
            }
          ),
          error = function(e) {
            error_message_rv(e$message)
          }
        )
      }, priority = 1)

      return_list <- list(
        error_r = shiny::reactive(!is.null(error_message_rv()))
      )

      return(return_list)
    }
  )
}
