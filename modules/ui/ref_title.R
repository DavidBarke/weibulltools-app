ref_title <- function(ref_name) {
  ref_html <- get_ref_html(ref_name)
  title <- xml2::xml_text(
    xml2::xml_find_all(ref_html, "//*[@class='page-header']/h1")
  )

  htmltools::h5(
    class = "flex-container flex-start",
    title,
    dropdown(
      label = shiny::icon("info-circle"),
      tooltip = "Help",
      dropdown_item("x", "Item 1"),
      dropdown_item("y", "Item 2")
    )
  )
}
