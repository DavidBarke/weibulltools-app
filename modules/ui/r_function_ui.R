r_function <- function(...,
                       name,
                       varname = NULL,
                       ref_name = name,
                       placeholder = "...",
                       collapsible = TRUE
) {
  varname <- if (!is.null(varname)) paste(varname, "<-")

  name <- htmltools::pre(
    class = "flex-container",
    varname,
    htmltools::a(
      class = "r-function-name",
      href = paste0(.globals$pkgdown$reference, ref_name),
      name
    ),
    htmltools::div(
      "("
    )
  )

  shiny::fluidRow(
    shiny::column(
      width = 12,
      class = "r-function",
      htmltools::div(
        class = "flex-container",
        name,
        if (collapsible) {
          shiny::actionButton(
            inputId = "xxx",
            label = NULL,
            icon = shiny::icon("chevron-down"),
            class = "up-down-btn"
          )
        }
      ),
      htmltools::div(
        class = "r-function-body",
        ...
      ),
      htmltools::div(
        class = "r-function-placeholder",
        style = "display: none",
        placeholder
      ),
      htmltools::pre(")")
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
        htmltools::tags$b(
          name
        )
      )
    ),
    shiny::column(
      width = 12 - width,
      ...
    )
  )
}

r_distribution_arg <- function(inputId, include3 = TRUE, width = 3) {
  choices <- c(
    "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev"
  )

  if (include3) {
    choices <- c(
      choices,
      "weibull3", "lognormal3", "loglogistic3"
    )
  }

  r_function_arg(
    name = "distribution",
    preSelectInput(
      inputId = inputId,
      label = NULL,
      choices = choices,
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
    ),
    width = width
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
    ),
    width = width
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
    ),
    width = width
  )
}

r_text_arg <- function(name, inputId, value, width = 3) {
  r_function_arg(
    name = name,
    htmltools::div(
      class = "pre",
      shiny::textInput(
        inputId = inputId,
        label = NULL,
        value = value,
        width = "100%"
      )
    ),
    width = width
  )
}

r_plot_method_arg <- function(inputId, width = 3) {
  r_function_arg(
    name = "plot_method",
    preSelectInput(
      inputId = inputId,
      label = NULL,
      choices = c("plotly", "ggplot2")
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

