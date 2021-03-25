dt_options <- function() {
  options(
    DT.options = list(
      pageLength = 10,
      dom = "lfpt",
      scrollX = TRUE
    )
  )
}

datatable_wrapper <- function(data, ...) {
  is_numeric_column <- purrr::map_lgl(names(data), ~ {
    !(. == "status") && is.numeric(data[[.]])
  })

  DT::datatable(data = data, ..., class = "display compact") %>%
    DT::formatSignif(
      # Add FALSE for row index column
      columns = c(FALSE, is_numeric_column),
      digits = 4
    )
}
