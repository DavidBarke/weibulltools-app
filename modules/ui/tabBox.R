add_connected_tabBox <- function(tabBox, id) {

  # Assumption that `side = "right"`
  tabBox$children[[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendAttributes(
    tabBox$children[[1]]$children[[1]]$children[[2]],
    `connected-id` = id
  )

  tabBox
}

append_ui <- function(tabBox, ui) {
  tabBox$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
    tabBox$children[[1]]$children[[2]],
    ui
  )

  tabBox
}
