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
