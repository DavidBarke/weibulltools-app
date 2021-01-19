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
    preSelectInput(
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

r_b_lives_arg <- function(inputId, width = 3) {
  choices <- c("0.01", "0.1", "0.5")

  r_function_arg(
    name = "b_lives",
    preSelectInput(
      inputId = inputId,
      label = NULL,
      choices = choices,
      multiple = TRUE,
      selected = choices,
      width = "100%"
    )
  )
}

r_bounds_arg <- function(inputId, width = 3) {
  choices <- c("two_sided", "lower", "upper")

  r_function_arg(
    name = "bounds",
    preSelectInput(
      inputId = inputId,
      label = NULL,
      choices = choices,
      width = "100%"
    )
  )
}

r_direction_arg <- function(inputId, width = 3) {
  r_function_arg(
    name = "direction",
    preSelectInput(
      inputId = inputId,
      label = NULL,
      choices = c("y", "x"),
      width = "100%"
    )
  )
}

preSelectInput <- function(inputId,
                           label,
                           choices,
                           selected = NULL,
                           multiple = FALSE,
                           selectize = TRUE,
                           width = "100%",
                           size = NULL
) {
  htmltools::div(
    class = "pre",
    shiny::selectInput(
      inputId = inputId,
      label = label,
      choices = choices,
      selected = selected,
      multiple = multiple,
      selectize = selectize,
      width = width,
      size = size
    )
  )
}

