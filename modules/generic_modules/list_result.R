list_result_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::textOutput(
      outputId = ns("error")
    ),
    shiny::textOutput(
      outputId = ns("warning")
    ),
    shiny::uiOutput(
      outputId = ns("list")
    ) %>% shinycssloaders::withSpinner()
  )
}

list_result_server <- function(id, .values, obj_r, dynamic = FALSE) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      server_env <- new.env()

      error_message_rv <- shiny::reactiveVal(character())
      warning_message_rv <- shiny::reactiveVal(character())

      output$error <- shiny::renderText({
        error_message_rv()
      })

      output$warning <- shiny::renderText({
        paste(warning_message_rv(), collapse = "\n")
      })

      names_r <- shiny::reactive({
        env <- environment()
        x <- character()
        tryCatch(
          withCallingHandlers(
            {
              # Clean up error and warning messages
              error_message_rv(character())
              warning_message_rv(character())
              # Assign names if no errors occured
              env$x <- names(obj_r())
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
            character()
          }
        )
        x
      })

      names_rv <- shiny::reactiveVal(character())

      shiny::observeEvent(names_r(), {
        # Set names of obj_r if modified
        if (!identical(names_r(), names_rv())) {
          names_rv(names_r())
        }
      }, ignoreInit = TRUE)

      # Only update items when names change
      items_r <- shiny::eventReactive(names_rv(), {
        items <- purrr::map(names_r(), function(name) {
          output_name <- "item" %_% name

          item_type <- get_item_type(obj_r()[[name]])

          if (item_type == "list") {
            out <- list_result_ui(
              id = ns(output_name)
            )

            # Call list_result_server exactly once
            if (!output_name %in% names(server_env)) {
              server_env[[output_name]] <- list_result_server(
                id = output_name,
                .values = .values,
                obj_r = shiny::reactive(obj_r()[[name]]),
                dynamic = FALSE
              )
            }
          } else {
            renderFun <- switch(
              item_type,
              "table" = DT::renderDataTable,
              "object" = shiny::renderPrint
            )

            outFun <- switch(
              item_type,
              "table" = DT::dataTableOutput,
              "object" = shiny::verbatimTextOutput
            )

            out <- outFun(
              outputId = ns(output_name)
            )

            # Call renderFun exactly once
            if (!output_name %in% names(output)) {
              output[[output_name]] <- renderFun({
                obj_r()[[name]]
              })
            }
          }

          bs4Dash::box(
            width = 12,
            collapsed = TRUE,
            title = name,
            out
          )
        })

        items
      })

      output$list <- shiny::renderUI({
        items <- if (dynamic) {
          items_r()
        } else {
          shiny::isolate(items_r())
        }

        shiny::fluidRow(items)
      })
    }
  )
}

get_item_type <- function(item) {
  if (is.data.frame(item)) return("table")
  if (is.list(item)) return("list")
  "object"
}
