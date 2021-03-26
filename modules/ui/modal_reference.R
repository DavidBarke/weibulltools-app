open_modal_reference <- function(ref_name) {
  reference <- get_reference(ref_name)

  shiny::showModal(shiny::modalDialog(
    title = paste("Reference:", ref_name),
    easyClose = TRUE,
    size = "l",
    reference
  ))
}
