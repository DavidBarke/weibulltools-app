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
                    pkgdown = "https://tim-tu.github.io/weibulltools/reference/"
) {
  purrr::walk(paste0(references, ".html"), function(ref) {
    curl::curl_download(
      url = paste0(pkgdown, ref),
      destfile = file.path(dir, ref)
    )
  })
}
