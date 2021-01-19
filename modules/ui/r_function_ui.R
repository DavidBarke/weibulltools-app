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

r_distribution_arg <- function(inputId, width = 3) {
  r_function_arg(
    name = "distribution",
    shiny::selectInput(
      inputId = inputId,
      label = NULL,
      choices = c(
        "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev",
        "weibull3", "lognormal3", "loglogistic3"
      ),
      width = "100%"
    ),
    width = width
  )
}

r_conf_level_arg <- function(inputId, width = 3) {
  r_function_arg(
    name = "conf_level",
    shiny::numericInput(
      inputId = inputId,
      label = NULL,
      value = 0.95,
      min = 0,
      max = 1,
      step = 0.01,
      width = "100%"
    ),
    width = width
  )
}

