list_result_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::uiOutput(
    outputId = ns("list")
  ) %>% shinycssloaders::withSpinner()
}

list_result_server <- function(id, .values, obj_r, dynamic = FALSE) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      server_env <- new.env()

      items_r <- shiny::eventReactive(obj_r(), {
        items <- purrr::map(names(obj_r()), function(name) {
          output_name <- "item" %_% name

          item_type <- get_item_type(obj_r()[[name]])

          if (item_type == "list") {
            out <- list_result_ui(
              id = ns(output_name)
            )

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
