r_function <- function(...,
                        name,
                        varname = NULL,
                        ref_name = name,
                        placeholder = "...",
                        collapsible = TRUE

) {
  varname <- if (!is.null(varname)) htmltools::tagList(
    varname,
    "<-"
  )

  name <- htmltools::pre(
    class = "flex-container",
    varname,
    htmltools::a(
      href = paste0("https://tim-tu.github.io/weibulltools/reference/", ref_name),
      target = "_blank",
      htmltools::span(
        class = "r-function-name emphasizeable",
        name = name,
        `data-toggle`="tooltip-hover",
        `data-placement` = "right",
        title = "Open documentation in new tab",
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
      class = "r-function emphasizeable",
      ref_title(ref_name),
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

r_function_varname <- function(varname) {
  htmltools::span(
    class = "r-function-varname emphasizeable",
    name = varname,
    varname
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
