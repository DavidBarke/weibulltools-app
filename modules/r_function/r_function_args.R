r_distribution_arg <- function(inputId, include3 = TRUE, width = 3) {
  choices <- distributions(include3)

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
    preNumericInput(
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
  choices <- "0.01, 0.1, 0.5"

  r_text_arg(
    name = "b_lives",
    inputId = inputId,
    value = choices,
    placeholder = "Enter values between 0 and 1 seperated by a comma..."
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

r_text_arg <- function(name, inputId, value, placeholder = NULL, width = 3) {
  r_function_arg(
    name = name,
    htmltools::div(
      class = "pre",
      shiny::textInput(
        inputId = inputId,
        label = NULL,
        value = value,
        width = "100%",
        placeholder = placeholder
      )
    ),
    width = width
  )
}

r_k_arg <- function(inputId, width = 3) {
  r_function_arg(
    name = "k",
    preNumericInput(
      inputId = inputId,
      label = NULL,
      value = 2,
      min = 1,
      max = NA,
      step = 1,
      width = "100%"
    ),
    width = width
  )
}

r_n_iter_arg <- function(inputId, width = 3) {
  r_function_arg(
    name = "n_iter",
    preNumericInput(
      inputId = inputId,
      label = NULL,
      value = 100L,
      min = 1,
      max = NA,
      step = 1,
      width = "100%"
    ),
    width = width
  )
}

r_conv_limit_arg <- function(inputId, width = 3) {
  r_function_arg(
    name = "conv_limit",
    preNumericInput(
      inputId = inputId,
      label = NULL,
      value = 1e-06,
      min = 0,
      max = NA,
      step = 1e-07,
      width = "100%"
    ),
    width = width
  )
}

r_diff_loglik_arg <- function(inputId, width = 3) {
  r_function_arg(
    name = "diff_loglik",
    preNumericInput(
      inputId = inputId,
      label = NULL,
      value = 0.01,
      min = 0,
      max = NA,
      step = 0.001,
      width = "100%"
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
