b_lives_modal_ui <- function(id, value) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::textInput(
      inputId = ns("b_lives"),
      label = NULL,
      placeholder = "0.01, 0.1, 0.5",
      value = value
    ),
    shiny::uiOutput(
      outputId = ns("error")
    )
  )
}

b_lives_modal_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      no_ws_r <- shiny::reactive({
        stringr::str_replace_all(input$b_lives, "\\s", "")
      })

      parsable_r <- shiny::reactive({
        stringr::str_detect(no_ws_r(), "^(\\d+(\\.\\d*)?,)*\\d+(\\.\\d*)?$")
      })

      b_lives_r <- shiny::reactive({
        shiny::req(parsable_r())
        as.numeric(stringr::str_split(no_ws_r(), ",")[[1]]) %>%
          unique() %>%
          sort()
      })

      all_valid_r <- shiny::reactive({
        all(b_lives_r() >= 0 & b_lives_r() <= 1)
      })

      output$error <- shiny::renderUI({
        shiny::validate(
          shiny::need(
            parsable_r(),
            "Please enter comma-separated numeric values between 0 and 1."
          ),
          shiny::need(
            all_valid_r(),
            "All entered values must be between 0 and 1."
          )
        )

        NULL
      })

      error_r <- shiny::reactive({
        !parsable_r() || !all_valid_r()
      })

      return_list <- list(
        b_lives_r = b_lives_r,
        error_r = error_r
      )
    }
  )
}
