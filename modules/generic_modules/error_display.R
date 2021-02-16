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

      error_message_rv <- shiny::reactiveVal(NULL)
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

      safe_obj_r <- shiny::reactive({
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
        error_r = shiny::reactive(!is.null(error_message_rv())),
        obj_r = safe_obj_r
      )

      return(return_list)
    }
  )
}
