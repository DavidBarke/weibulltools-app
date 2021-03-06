dataset_ui <- function(id, ref_name) {
  ns <- shiny::NS(id)

  ref_html <- get_ref_html(ref_name)

  contents <- xml2::xml_find_all(ref_html, "//div[contains(@class, 'contents')]")

  shiny::fluidRow(
    bs4Dash::box(
      width = 6,
      solidHeader = TRUE,
      status = "primary",
      title = "Description",
      xml2html(contents)
    ),
    bs4Dash::box(
      width = 6,
      solidHeader = TRUE,
      status = "primary",
      title = "Data",
      DT::dataTableOutput(
        outputId = ns("data")
      )
    )
  )
}

dataset_server <- function(id, .values, dataset) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$data <- DT::renderDataTable({
        datatable_wrapper(get(dataset, "package:weibulltools"))
      })
    }
  )
}
