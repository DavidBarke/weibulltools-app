numeric_input <- function(inputId,
                          label,
                          value,
                          min = NA,
                          max = NA,
                          step = NA,
                          width = "100%"
) {
  htmltools::div(
    class = "flex-container",
    numericInput(
      inputId = inputId,
      label = label,
      value = value,
      min = NA,
      max = NA,
      step = NA,
      width = width
    ),
    htmltools::span(
      `data-toggle` = "tooltip-hover",
      title = paste("Reset to default:", value),
      shiny::actionLink(
        inputId = inputId %_% "reset",
        label = NULL,
        icon = shiny::icon("undo-alt"),
        class = "reset-numeric-input"
      )
    )
  )
}
