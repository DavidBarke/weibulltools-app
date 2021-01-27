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

ref_dropdown <- function(id, varname, ref_tbl) {
  stopifnot(all(c("label", "reference", "tabName") %in% names(ref_tbl)))

  dropdown_items <- purrr::pmap(ref_tbl, function(label, reference, tabName) {
    htmltools::a(
      label,
      class = "dropdown-item ref-link",
      `tab-name` = tabName,
      reference = reference
    )
  })

  htmltools::tagList(
    dropdown(
      label = varname,
      dropdown_items
    ),
    htmltools::tags$script("bindEmphasizeReferences();")
  )
}
