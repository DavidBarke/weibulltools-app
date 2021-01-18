r_function <- function(name, ...) {
  shiny::fluidRow(
    shiny::column(
      width = 12,
      htmltools::div(
        class = "r-function",
        htmltools::pre(paste0(name, "(")),
        htmltools::div(
          class = "r-function-body",
          ...
        ),
        htmltools::pre(")")
      )
    )
  )
}

r_function_arg <- function(name, ..., width = 3) {
  shiny::fluidRow(
    class = "r-function-arg",
    shiny::column(
      width = width,
      htmltools::tags$pre(
        class = "vertical-center",
        name
      )
    ),
    shiny::column(
      width = 12 - width,
      ...
    )
  )
}

