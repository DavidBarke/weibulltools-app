datasets_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    bs4Dash::tabBox(
      id = ns("dataset_tabs"),
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      type = "tabs",
      side = "right",
      shiny::tabPanel(
        title = "alloy",
        dataset_ui(
          id = ns("alloy"),
          ref_name = "alloy"
        )
      ),
      shiny::tabPanel(
        title = "shock",
        dataset_ui(
          id = ns("shock"),
          ref_name = "shock"
        )
      ),
      shiny::tabPanel(
        title = "voltage",
        dataset_ui(
          id = ns("voltage"),
          ref_name = "voltage"
        )
      ),
      shiny::tabPanel(
        title = "field_data",
        dataset_ui(
          id = ns("field_data"),
          ref_name = "field_data"
        )
      )
    )
  )
}

datasets_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      dataset_server(
        id = "alloy",
        .values = .values,
        dataset = "alloy"
      )

      dataset_server(
        id = "shock",
        .values = .values,
        dataset = "shock"
      )

      dataset_server(
        id = "voltage",
        .values = .values,
        dataset = "voltage"
      )

      dataset_server(
        id = "field_data",
        .values = .values,
        dataset = "field_data"
      )
    }
  )
}
