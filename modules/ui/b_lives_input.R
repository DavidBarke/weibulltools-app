b_lives_input <- function(inputId, value) {
  htmltools::div(
    id = inputId,
    class = "b-lives-input",
    `data-value` = paste(value, collapse = ",")
  )
}

update_b_lives_input <- function(inputId,
                                 value,
                                 session = shiny::getDefaultReactiveDomain()
) {
  message <- list(value = value)

  session$sendInputMessage(inputId, message)
}
