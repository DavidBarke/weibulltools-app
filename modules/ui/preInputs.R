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


preNumericInput <- function(inputId,
                            label,
                            value,
                            min = NA,
                            max = NA,
                            step = NA,
                            width = "100%"
) {
  htmltools::div(
    class = "pre",
    shiny::numericInput(
      inputId = inputId,
      label = label,
      value = value,
      min = min,
      max = max,
      step = step,
      width = width
    )
  )
}
