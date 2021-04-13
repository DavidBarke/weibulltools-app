b_lives_ui <- function(id, width = 3) {
  ns <- shiny::NS(id)

  r_function_arg(
    name = "b_lives",
    shiny::actionLink(
      inputId = ns("change"),
      label = b_lives_input(
        inputId = ns("b_lives"),
        value = c(0.01, 0.1, 0.5)
      )
    ),
    width = width
  )
}

b_lives_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      shiny::observeEvent(input$change, {
        shiny::showModal(shiny::modalDialog(
          title = "Change b_lives",
          easyClose = TRUE,
          b_lives_modal_ui(
            id = ns("b_lives_modal"),
            value = paste(input$b_lives, collapse = ", ")
          ),
          footer = htmltools::tagList(
            shiny::uiOutput(
              outputId = ns("confirm_change")
            ),
            shiny::modalButton("Dismiss")
          )
        ))
      })

      output$confirm_change <- shiny::renderUI({
        if (!b_lives_modal_return$error_r()) {
          shiny::actionButton(
            inputId = ns("confirm_change"),
            label = "Confirm"
          )
        }
      })

      shiny::observeEvent(input$confirm_change, {
        shiny::removeModal()

        update_b_lives_input(
          inputId = "b_lives",
          value = b_lives_modal_return$b_lives_r()
        )
      })

      b_lives_modal_return <- b_lives_modal_server(
        id = "b_lives_modal",
        .values = .values
      )

      return_list <- list(
        b_lives_r = shiny::reactive(input$b_lives)
      )

      return(return_list)
    }
  )
}
