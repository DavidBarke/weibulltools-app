`%||%` <- function(x, y) if (!(is.null(x) || is.na(x))) x else y

`%_%` <- function(x, y) paste(x, y, sep = "_")

`%<-%` <- function(x, y) {
  for (name in names(y)) {
    x[[name]] <- y[[name]]
  }

  x
}

extract_nums <- function(x, sep = ",") {
  num_str <- as.numeric(trimws(unlist(strsplit(x, sep))))
  num_str[!is.na(num_str)]
}

format_vector <- function(x) {
  quo <- if (is.character(x)) '"' else ""
  if (length(x) == 1) {
    paste0(quo, x, quo)
  } else {
    paste0("c(", paste0(quo, x, quo, collapse = ", "), ")")
  }
}

format_list <- function(x) {
  if (length(x) == 0) return("list()")
  names_x <- names(x)
  if (is.null(names_x) || any(names_x == "")) stop("x must be a named list!")

  entries <- purrr::map_chr(names_x, function(name) {
    formatter <- if (is.list(x[[name]])) format_list else format_vector
    paste(name, "=", formatter(x[[name]]))
  })

  paste0("list(", paste0(entries, collapse = ", ") ,")")
}

replace_comma <- function(x) {
  stringr::str_replace_all(x, ",", "@@")
}

distributions <- function(include_thres = TRUE) {
  x <- c(
    "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev",
    "exponential"
  )

  if (include_thres) {
    x <- c(
      x,
      "weibull3", "lognormal3", "loglogistic3", "exponential2"
    )
  }

  x
}

std_distribution <- function(distribution) {
  stringr::str_replace(distribution, "\\d", "")
}

compatible_distributions <- function(distribution) {
  if (distribution %in% c("weibull", "lognormal", "loglogistic")) {
    return(c(distribution, paste0(distribution, "3")))
  }

  if (distribution == "exponential") {
    return(c("exponential", "exponential2"))
  }

  distribution
}

r_code <- function(...) {
  htmltools::pre(
    class = "r",
    htmltools::code(paste(..., sep = "\n"))
  )
}
