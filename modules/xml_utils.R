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

as_xml_nodeset <- function(...) {
  dots <- list(...)
  purrr::walk(
    dots,
    function(x) if (!inherits(x, "xml_node")) stop("x must be a xml node!")
  )

  structure(
    dots,
    class = "xml_nodeset"
  )
}

xml2html <- function(x) {
  tmp <- tempfile(fileext = ".html")
  xml2::write_html(x, tmp)
  htmltools::includeHTML(tmp)
}

get_reference <- function(ref_name) {
  ref <- get_ref_html(ref_name)
  x <- xml2::xml_find_all(ref, "//div[contains(@class,'contents')]")
  xml2::xml_attr(x, "class") <- "contents" # remove class 'col-md-9'
  xml2html(x)
}

has_id <- function(x) {
  !is.na(xml2::xml_text(xml2::xml_find_first(x, "@id")))
}

get_ids <- function(x) {
  xml2::xml_text(xml2::xml_find_all(x, "@id"))
}




