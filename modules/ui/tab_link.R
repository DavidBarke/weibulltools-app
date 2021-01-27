varname_link <- function(tabName, varname) {
  htmltools::tagList(
    htmltools::a(
      href = "#",
      htmltools::span(
        class = "varname-link",
        `data-toggle` = "tooltip-hover",
        `data-placement` = "right",
        title = "Show definition",
        varname = varname,
        `tab-name` = tabName,
        varname
      )
    ),
    htmltools::tags$script("bindTooltipHover();"),
    htmltools::tags$script("bindEmphasizeable();")
  )
}
