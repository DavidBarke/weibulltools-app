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
      target = "_blank",
      htmltools::span(
        `data-toggle`="tooltip-hover",
        `data-placement` = "right",
        title = "Open reference in new tab",
        name
      )
    ),
    htmltools::div(
      "("
    )
  )

  r_function_args <- list(...)
  names(r_function_args) <- as.character(purrr::map_if(
    r_function_args,
    function(x) exists("name", attributes(x) %||% list()),
    ~ attr(., "name", exact = TRUE),
    .else = ~ NA
  ))

  # Argument documentation sourced from pkgdown page
  arg_text <- params_text(ref_name)

  r_function_args <- purrr::map2(r_function_args, names(r_function_args), function(arg, arg_name) {
    if (arg_name %in% names(arg_text)) {
      arg(arg_text[[arg_name]])
    } else {
      arg()
    }
  })

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
        r_function_args
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

r_function_arg <- function(name, ..., standalone = FALSE, width = 3) {
  arg_fun <- function(arg_text = NULL) {
    name <- if (is.null(arg_text)) {
      name
    } else {
      htmltools::span(
        `data-toggle`="tooltip-hover",
        title = arg_text,
        name
      )
    }

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

  if (standalone) return(arg_fun())

  attr(arg_fun, "name") <- name

  arg_fun
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
    shiny::numericInput(
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
    shiny::numericInput(
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
    shiny::numericInput(
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
    shiny::numericInput(
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
