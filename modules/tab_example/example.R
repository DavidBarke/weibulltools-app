example_ui <- function(id) {
  id <- shiny::NS(id)

  htmltools::tags$iframe(
    id = "comprehensive-example",
    src = "https://weibulltools.org/rmd/comprehensive_example.Rmd",
    scrolling = "no"
  )
}

example_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      js$bindResizeIframe(id = "comprehensive-example", asis = TRUE)
    }
  )
}
