dt_options <- function() {
  options(
    DT.options = list(
      pageLength = 10,
      dom = "lfpt",
      scrollX = TRUE
    )
  )
}

datatable_wrapper <- function(...) {
  DT::datatable(..., class = "display compact")
}
