qf_mixmod_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::div(
    id = ns("container"),
    class = "btn-list"
  )
}

qf_mixmod_server <- function(id,
                             .values,
                             error_message_r,
                             k_id,
                             k_session
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      quick_fix_shown_rv <- shiny::reactiveVal(FALSE)

      is_error_r <- shiny::reactive({
        !is.null(error_message_r())
      })

      is_not_voltage_r <- shiny::reactive({
        .values$reliability_data_session$input[[
          .values$reliability_data_data_id
        ]] != "voltage"
      })

      shiny::observeEvent(error_message_r(), {
        if (!is_error_r()) {
          quick_fix_shown_rv(FALSE)

          shiny::removeUI(
            selector = paste0("#", ns("container"), " *"),
            multiple = TRUE
          )
        } else {
          if (quick_fix_shown_rv()) return()

          quick_fix_shown_rv(TRUE)
          shiny::insertUI(
            selector = paste0("#", ns("container")),
            where = "afterBegin",
            ui = htmltools::tagList(
              if (is_not_voltage_r()) {
                shiny::actionButton(
                  inputId = ns("use_voltage"),
                  label = htmltools::tagList(
                    "Quick fix: Use dataset",
                    htmltools::code("voltage"),
                    "with",
                    htmltools::code("k = 2"),
                    "instead"
                  ),
                  icon = shiny::icon("wrench")
                )
              },
              shiny::actionButton(
                inputId = ns("k_1"),
                label = htmltools::tagList(
                  "Quick Fix: Set",
                  htmltools::code("k"),
                  "to 1 (no mixture model)"
                )
              ),
              shiny::actionButton(
                inputId = ns("reduce_k"),
                label = htmltools::tagList(
                  "Quick Try: Reduce",
                  htmltools::code("k"),
                  "by 1"
                ),
                icon = shiny::icon("wrench")
              )
            )
          )
        }
      }, ignoreNULL = FALSE)

      shiny::observeEvent(input$use_voltage, {
        bs4Dash::toast(
          title = "Quick Fix",
          body = "Set dataset to \"voltage\"",
          options = .values$toast_options()
        )

        shiny::updateSelectInput(
          inputId = .values$reliability_data_data_id,
          session = .values$reliability_data_session,
          selected = "voltage"
        )

        shiny::updateNumericInput(
          inputId = k_id,
          session = k_session,
          value = 2
        )
      })

      shiny::observeEvent(input$k_1, {
        bs4Dash::toast(
          title = "Quick Fix",
          body = "Set k to 1",
          options = .values$toast_options()
        )

        shiny::updateNumericInput(
          inputId = k_id,
          session = k_session,
          value = 1
        )
      })

      shiny::observeEvent(input$reduce_k, {
        bs4Dash::toast(
          title = "Quick Try",
          body = "Reduced k by 1",
          options = .values$toast_options()
        )

        shiny::updateNumericInput(
          inputId = k_id,
          session = k_session,
          value = max(1, k_session$input[[k_id]] - 1)
        )
      })
    }
  )
}
