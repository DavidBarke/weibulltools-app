get_ref_html <- function(ref_name, quiet = FALSE) {
  ref <- paste0(ref_name, ".html")
  xml2::read_html(file.path("reference", ref))
}

params_text <- function(ref_name) {
  ref_html <- get_ref_html(ref_name)
  arg_tr <- xml2::xml_find_all(ref_html, "//table[@class='ref-arguments']/tr")
  x <- xml2::xml_text(xml2::xml_find_all(arg_tr, "//td/p"))
  names(x) <- xml2::xml_text(xml2::xml_find_all(arg_tr, "//th"))
  x
}

ref_title <- function(ref_name) {
  ref_html <- get_ref_html(ref_name)
  xml2::xml_text(xml2::xml_find_all(ref_html, "//*[@class='page-header']/h1"))
}

xml2html <- function(x) {
  tmp <- tempfile(fileext = ".html")
  xml2::write_html(x, tmp)
  htmltools::includeHTML(tmp)
}

`%||%` <- function(x, y) if (!is.null(x)) x else y

`%_%` <- function(x, y) paste(x, y, sep = "_")

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

distributions <- function(include3 = TRUE) {
  x <- c(
    "weibull", "lognormal", "loglogistic", "normal", "logistic", "sev"
  )

  if (include3) {
    x <- c(
      x,
      "weibull3", "lognormal3", "loglogistic3"
    )
  }

  x
}

compatible_distributions <- function(distribution) {
  if (distribution %in% c("weibull", "lognormal", "loglogistic")) {
    c(distribution, paste0(distribution, "3"))
  } else {
    distribution
  }
}

r_code <- function(...) {
  htmltools::pre(
    class = "r",
    htmltools::code(paste(..., sep = "\n"))
  )
}
