varname_link_ui <- function(id, name) {
  tab_link_ui(id, name, "Show definition")
}

tab_link_ui <- function(id, name, tooltip = NULL) {
  ns <- shiny::NS(id)

  if (!is.null(name)) {
    name <- htmltools::span(
      `data-toggle` = "tooltip-hover",
      `data-placement` = "right",
      title = tooltip,
      name
    )
  }

  htmltools::tagList(
    shiny::actionLink(
      inputId = ns("link"),
      label = name
    ),
    htmltools::tags$script("bindTooltipHover();")
  )
}

tab_link_server <- function(id, .values, tabName) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      shiny::observeEvent(input$link, {
        .values$update_sidebar(tabName)
      })
    }
  )
}

varname_link_server <- tab_link_server
