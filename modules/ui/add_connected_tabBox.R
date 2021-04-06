add_connected_tabBox <- function(tabBox, id) {

  # Assumption that `side = "right"`
  tabBox$children[[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendAttributes(
    tabBox$children[[1]]$children[[1]]$children[[2]],
    `connected-id` = id
  )

  tabBox
}
