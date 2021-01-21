params_text <- function(ref_name) {
  ref_html <- xml2::read_html(paste0(.globals$pkgdown$reference, ref_name))
  arg_tr <- xml2::xml_find_all(ref_html, "//table[@class='ref-arguments']/tr")
  x <- xml2::xml_text(xml2::xml_find_all(arg_tr, "//td/p"))
  names(x) <- xml2::xml_text(xml2::xml_find_all(arg_tr, "//th"))
  x
}

xml2html <- function(x) {
  tmp <- tempfile(fileext = ".html")
  xml2::write_html(x, tmp)
  htmltools::includeHTML(tmp)
}

`%||%` <- function(x, y) if (!is.null(x)) x else y

`%_%` <- function(x, y) paste(x, y, sep = "_")
