ref_title <- function(ref_name) {
  ref_html <- get_ref_html(ref_name)
  title <- xml2::xml_text(
    xml2::xml_find_all(ref_html, "//*[@class='page-header']/h1")
  )

  htmltools::h5(
    class = "flex-container flex-start",
    title,
    htmltools::tags$span(
      class = "info-circle",
      onclick = '
      console.log("click");
      Shiny.setInputValue(
        "info_circle",
        {
          nonce: Math.random(),
          value: $(this).attr("data-value")
        }
      );
      ',
      `data-value` = ref_name,
      `data-toggle` = "tooltip-hover",
      title = "Open documentation in modal dialog",
      shiny::icon("info-circle")
    )
  )
}
