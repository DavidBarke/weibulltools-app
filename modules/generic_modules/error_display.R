error_display_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("error")
    ),
    shiny::uiOutput(
      outputId = ns("warning")
    )
  )
}

error_display_server <- function(id, .values, obj_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      # Start with error, otherwise code that is dependent on the error_r return
      # of this module fails (and so does the application) before tryCatch of
      # this module was executed
      error_message_rv <- shiny::reactiveVal("")
      warning_message_rv <- shiny::reactiveVal(character())

      output$error <- shiny::renderUI({
        if (!is.null(error_message_rv())) {
          htmltools::tagList(
            htmltools::h5("The following error was recorded:"),
            htmltools::pre(error_message_rv())
          )
        }
      })

      output$warning <- shiny::renderUI({
        len <- length(warning_message_rv())
        heading <- if (len == 1) {
          htmltools::h5("The following warning was recorded:")
        } else if (len > 1) {
          htmltools::h5("The following warnings were recorded:")
        }

        warning_items <- purrr::map(warning_message_rv(), function(message) {
          htmltools::tags$pre(message)
        })

        if (len) {
          htmltools::tagList(
            heading,
            warning_items
          )
        }
      })

      shiny::observe({
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
      })

      return_list <- list(
        error_r = shiny::reactive(!is.null(error_message_rv()))
      )

      return(return_list)
    }
  )
}
