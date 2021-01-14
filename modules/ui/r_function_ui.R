r_function <- function(name, ...) {
  dots <- list(...)

  indices <- seq_len(2 * length(dots) - 1)
  ui <- purrr::map(indices, function(index) {
    if (index %% 2) {
      dots[[as.integer(index / 2) + 1]]
    } else {
      htmltools::hr()
    }
  })

  htmltools::div(
    class = "r-function",
    htmltools::pre(paste0(name, "(")),
    ui,
    htmltools::pre(")")
  )
}

r_function_arg <- function(name, ...) {
  shiny::fluidRow(
    class = "r-function-arg",
    shiny::column(
      width = 4,
      htmltools::tags$pre(
        class = "vertical-center",
        name
      )
    ),
    shiny::column(
      width = 8,
      ...
    )
  )
}
