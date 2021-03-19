text_box <- function(..., title, width = 12) {
  bs4Dash::box(
    width = width,
    title = title,
    solidHeader = TRUE,
    status = "primary",
    htmltools::div(
      class = "text-container",
      ...
    )
  )
}
