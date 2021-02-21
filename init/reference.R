get_references <- function() {
  c(
    "reliability_data",
    "mcs_delay_data",
    "mcs_mileage_data",
    "estimate_cdf",
    "plot_prob",
    "ml_estimation",
    "loglik_function",
    "loglik_profiling",
    "rank_regression",
    "r_squared_profiling",
    "mixmod_em",
    "mixmod_regression",
    "predict_prob",
    "predict_quantile",
    "plot_mod",
    "plot_pop",
    "confint_betabinom",
    "confint_fisher",
    "delta_method",
    "plot_conf",
    "dist_delay",
    "mcs_delay",
    "dist_mileage",
    "mcs_mileage",
    "alloy",
    "shock",
    "voltage",
    "field_data"
  )
}

download_reference <- function(
                    dir,
                    references = get_references(),
                    pkgdown = "https://tim-tu.github.io/weibulltools/reference"
) {
  purrr::walk(paste0(references, ".html"), function(ref) {
    curl::curl_download(
      url = paste0(pkgdown, "/", ref),
      destfile = file.path(dir, ref)
    )
  })
}


replace_links <- function(dir,
                          pkgdown = "https://tim-tu.github.io/weibulltools"
) {
  ref_files <- list.files(dir, pattern = "\\.html$")
  ref_paths <- file.path(dir, ref_files)

  purrr::walk(ref_paths, function(path) {
    ref_html <- xml2::read_html(path)

    hrefs <- ref_html %>% xml2::xml_find_all("//a") %>% xml2::xml_attr("href")

    hrefs <- purrr::map_chr(hrefs, function(href) {
      if ( stringr::str_detect(href, "^\\.\\.*")) {
        href <- stringr::str_replace(href, "^\\.\\.", pkgdown)
      } else if (stringr::str_detect(href, "^[A-Za-z_]*\\.html")) {
        href <- paste0(pkgdown, "/reference/", href)
      }

      href
    })

    a <- ref_html %>% xml2::xml_find_all("//a")
    xml2::xml_attr(a, "href") <- hrefs

    xml2::write_html(ref_html, path)
  })
}


download_pkgdown <- function(dir,
                            pkgdown = "https://tim-tu.github.io/weibulltools"
) {
  pkgdown_reference <- paste0(pkgdown, "/reference")

  download_reference(dir, get_references(), pkgdown_reference)

  replace_links(dir, pkgdown)
}
