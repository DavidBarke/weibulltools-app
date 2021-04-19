ref_title <- function(ref_name) {
  ref_html <- get_ref_html(ref_name)
  title <- xml2::xml_text(
    xml2::xml_find_all(ref_html, "//*[@class='page-header']/h1")
  )

  htmltools::h5(
    class = "flex-container flex-start",
    title,
    htmltools::a(
      class = "external-link",
      href = paste0("https://tim-tu.github.io/weibulltools/reference/", ref_name),
      target = "_blank",
      `data-toggle`="tooltip-hover",
      `data-placement` = "right",
      title = "Open documentation in new tab",
      shiny::icon("external-link-alt")
    )
  )
}
