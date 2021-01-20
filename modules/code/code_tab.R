code_tab_ui <- function(id, title) {
  ns <- NS(id)

  shiny::tabPanel(
    title = title,
    code_ui(
      id = ns("code")
    )
  )
}

code_tab_server <- function(id, .values, obj_r) {
  shiny::moduleServer(
    id = id,
    function(input, output, session) {
      ns <- session$ns

      code_server(
        id = "code",
        .values = .values,
        obj_r = obj_r
      )
    }
  )
}
