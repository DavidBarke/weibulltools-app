list_result_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    error_display_ui(
      id = ns("error_display")
    ),
    # Container for list elements
    htmltools::div(
      id = ns("list"),
      class = "list-result"
    )
  )
}

list_result_server <- function(id, .values, obj_r, dynamic = FALSE) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      server_env <- new.env()

      names_r <- shiny::reactive({
        if (error_display_return$error_r()) {
          character()
        } else {
          names(error_display_return$obj_r())
        }
      })

      names_rv <- shiny::reactiveVal(character())

      shiny::observeEvent(names_r(), {
        # Break early if names of existing and new list don't differ
        if (identical(names_r(), names_rv())) return()

        names_to_add <- setdiff(names_r(), names_rv())
        names_to_remove <- setdiff(names_rv(), names_r())

        # Remove all elements belonging to outdated names
        purrr::walk(names_to_remove, function(name) {
          shiny::removeUI(
            selector = glue::glue("#{id}_{name}", id = ns("list"), name = name)
          )
        })

        # Add elements for new names
        positions <- which(names_r() %in% names_to_add)
        purrr::walk2(names_to_add, positions, function(name, pos) {
          output_name <- "item" %_% name

          item_type <- get_item_type(obj_r()[[name]])

          if (item_type == "list") {
            ## Recursive step
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

              output[["title" %_% name]] <- shiny::renderUI({
                obj <- shiny::req(error_display_return$obj_r())
                htmltools::tagList(
                  bs4Dash::bs4Badge(name, color = "primary"),
                  glue::glue(
                    "{type} [{size}]",
                    type = pillar::type_sum(obj[[name]]),
                    size = length(obj[[name]])
                  )
                )
              })
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

            # Call renderFuns exactly once
            if (!output_name %in% names(output)) {
              output[[output_name]] <- renderFun({
                obj <- shiny::req(error_display_return$obj_r())
                obj[[name]]
              })

              output[["title" %_% name]] <- shiny::renderUI({
                obj <- shiny::req(error_display_return$obj_r())
                htmltools::tagList(
                  bs4Dash::bs4Badge(name, color = "primary"),
                  pillar::obj_sum(obj[[name]])
                )
              })
            }
          }

          item <- bs4Dash::box(
            width = 12,
            collapsed = TRUE,
            title = shiny::uiOutput(
              outputId = ns("title" %_% name)
            ),
            out
          )

          if (pos == 1) {
            selector <- paste0("#", ns("list"))
            where <- "afterBegin"
          } else {
            selector <- glue::glue(
              "#{id} > div:nth-child({n})",
              id = ns("list"),
              n = pos - 1
            )
            where <- "afterEnd"
          }

          shiny::insertUI(
            selector = selector,
            where = where,
            ui = htmltools::div(
              id = ns("list") %_% name,
              item
            )
          )
        })

        # Update names_rv
        names_rv(names_r())
      })

      error_display_return <- error_display_server(
        id = "error_display",
        .values = .values,
        obj_r = obj_r
      )

      return_list <- list(
        error_message_r = error_display_return$error_message_r
      )

      return(return_list)
    }
  )
}

get_item_type <- function(item) {
  if (is.data.frame(item)) return("table")
  if (is.list(item)) return("list")
  "object"
}
