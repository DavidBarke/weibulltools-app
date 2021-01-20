code_box_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    title = "Code",
    code_ui(
      id = ns("code")
    )
  )
}

code_box_server <- function(id, .values, obj_r) {
  shiny::moduleServer(
    id,
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
