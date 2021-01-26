dropdown <- function(..., label) {
  htmltools::div(
    class = "dropdown",
    `data-toggle` = "tooltip-hover",
    `data-placement` = "right",
    title = "Show references",
    htmltools::tags$button(
      class = "btn dropdown-toggle",
      type = "button",
      `data-toggle` = "dropdown",
      `data-boundary` = "window",
      `aria-haspopup` = "true",
      `aria-expanded` = "false",
      label
    ),
    htmltools::div(
      class = "dropdown-menu",
      `aria-labelledby` = "dropdownMenuButton",
      ...
    )
  )
}

dropdown_item <- function(inputId, label) {
  shiny::actionLink(
    inputId = inputId,
    label = label,
    class = "dropdown-item"
  )
}

ref_dropdown_ui <- function(id, varname, references) {
  ns <- shiny::NS(id)

  if (is.null(names(references))) names(references) <- references

  dropdown_items <- purrr::map2(names(references), references, function(ref, label) {
    dropdown_item(
      inputId = ns("link" %_% ref),
      label = label
    )
  })

  dropdown(
    label = varname,
    dropdown_items
  )
}

ref_dropdown_server <- function(id, .values, tabNames) {
  stopifnot(all(!is.na(names(tabNames))))

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      purrr::walk2(names(tabNames), tabNames, function(ref, tabName) {
        shiny::observeEvent(input[["link" %_% ref]], {
          .values$update_sidebar(tabName)
        })
      })
    }
  )
}
