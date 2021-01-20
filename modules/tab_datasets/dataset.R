dataset_ui <- function(id, ref_name) {
  ns <- shiny::NS(id)

  ref_html <- xml2::read_html(paste0(.globals$pkgdown$reference, ref_name))

  contents <- xml2::xml_find_all(ref_html, "//div[contains(@class, 'contents')]")

<<<<<<< HEAD
=======
<<<<<<< HEAD
  shiny::fluidRow(
    shiny::column(
      width = 6,
      xml2html(contents)
=======
>>>>>>> 7db7450... Add datasets tab.
  tmp <- tempfile(fileext = ".html")
  xml2::write_html(contents, tmp)

  contents <- htmltools::includeHTML(tmp)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      contents
<<<<<<< HEAD
=======
>>>>>>> 238d00b... Add datasets tab.
>>>>>>> 7db7450... Add datasets tab.
    ),
    shiny::column(
      width = 6,
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
        DT::datatable(get(dataset, "package:weibulltools"))
      })
    }
  )
}
