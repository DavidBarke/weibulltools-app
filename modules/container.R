container_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::bs4DashPage(
    header = bs4Dash::bs4DashNavbar(
      title = "weibulltools"
    ),
    sidebar = bs4Dash::bs4DashSidebar(
      width = "290px",
      skin = "light",
      bs4Dash::sidebarMenu(
        id = ns("sidebar"),
        bs4Dash::menuItem(
          text = "Getting Started",
          tabName = "overview"
        ),
        bs4Dash::menuItem(
          text = "Comprehensive Example",
          tabName = "complete"
        ),
        bs4Dash::menuItem(
          text = "Datasets",
          tabName = "datasets"
        ),
        bs4Dash::menuItem(
          text = "Reliability Data",
          tabName = "reliability_data",
          selected = TRUE
        ),
        bs4Dash::menuItem(
          text = "Non-Parametric Failure Probabilities",
          tabName = "probability_estimation"
        ),
        bs4Dash::menuItem(
          text = "Parametric Models",
          bs4Dash::menuSubItem(
            text = "ML Estimation",
            tabName = "ml_estimation"
          ),
          bs4Dash::menuSubItem(
            text = "Rank Regression",
            tabName = "rank_regression"
          ),
          bs4Dash::menuSubItem(
            text = "EM Algorithm",
            tabName = "mixmod_em"
          ),
          bs4Dash::menuSubItem(
            text = "Segmented Regression",
            tabName = "mixmod_regression"
          )
        ),
        bs4Dash::menuItem(
          text = "Confidence Intervals",
          bs4Dash::menuSubItem(
            text = "Beta Binomial",
            tabName = "confint_betabinom"
          ),
          bs4Dash::menuSubItem(
            text = "Fisher",
            tabName = "confint_fisher"
          )
        ),
        bs4Dash::menuItem(
          text = "Monte Carlo Simulation",
          bs4Dash::menuSubItem(
            text = "MCS for Delays",
            tabName = "mcs_delay"
          ),
          bs4Dash::menuSubItem(
            text = "MCS for Mileages",
            tabName = "mcs_mileage"
          )
        )
      )
    ),
    body = bs4Dash::bs4DashBody(
      bs4Dash::bs4TabItems(
        bs4Dash::bs4TabItem(
          tabName = "overview",
          overview_ui(
            id = ns("overview")
          )
        ),
        bs4Dash::bs4TabItem(
          tabName = "datasets",
          datasets_ui(
            id = ns("datasets")
          )
        ),
        bs4Dash::bs4TabItem(
          tabName = "reliability_data",
          reliability_data_ui(
            id = ns("reliability_data")
          )
        ),
        bs4Dash::bs4TabItem(
          tabName = "probability_estimation",
          probability_estimation_ui(
            id = ns("probability_estimation")
          )
        ),
        bs4Dash::bs4TabItem(
          tabName = "ml_estimation",
          ml_estimation_ui(
            id = ns("ml_estimation")
          )
        ),
        bs4Dash::bs4TabItem(
          tabName = "rank_regression",
          rank_regression_ui(
            id = ns("rank_regression")
          )
        ),
        bs4Dash::bs4TabItem(
          tabName = "mixmod_em",
          mixmod_em_ui(
            id = ns("mixmod_em")
          )
        ),
        bs4Dash::bs4TabItem(
          tabName = "mixmod_regression",
          mixmod_regression_ui(
            id = ns("mixmod_regression")
          )
        ),
        bs4Dash::tabItem(
          tabName = "confint_betabinom",
          confint_betabinom_ui(
            id = ns("confint_betabinom")
          )
        ),
        bs4Dash::tabItem(
          tabName = "confint_fisher",
          confint_fisher_ui(
            id = ns("confint_fisher")
          )
        ),
        bs4Dash::tabItem(
          tabName = "mcs_delay",
          mcs_delay_ui(
            id = ns("mcs_delay")
          )
        ),
        bs4Dash::tabItem(
          tabName = "mcs_mileage",
          mcs_mileage_ui(
            id = ns("mcs_mileage")
          )
        )
      )
    ),
    title = "weibulltools"
  )
}

container_server <- function(id, .values) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      .values$sidebar$session <- session
      .values$sidebar$id <- "sidebar"

      # Register function for updating sidebar from other modules
      .values$update_sidebar <- function(tabName) {
        bs4Dash::updateTabItems(
          session = .values$sidebar$session,
          inputId = .values$sidebar$id,
          selected = tabName
        )
      }

      overview_server(
        id = "overview",
        .values = .values
      )

      datasets_server(
        id = "datasets",
        .values = .values
      )

      reliability_data_return <- reliability_data_server(
        id = "reliability_data",
        .values = .values
      )

      probability_estimation_return <- probability_estimation_server(
        id = "probability_estimation",
        .values = .values,
        reliability_data_r = reliability_data_return$reliability_data_r
      )

      ml_estimation_return <- ml_estimation_server(
        id = "ml_estimation",
        .values = .values,
        reliability_data_r = reliability_data_return$reliability_data_r,
        plot_prob_r = probability_estimation_return$plot_prob_r
      )

      rank_regression_return <- rank_regression_server(
        id = "rank_regression",
        .values = .values,
        estimate_cdf_r = probability_estimation_return$estimate_cdf_r,
        plot_prob_r = probability_estimation_return$plot_prob_r
      )

      mixmod_em_return <- mixmod_em_server(
        id = "mixmod_em",
        .values = .values,
        reliability_data_r = reliability_data_return$reliability_data_r
      )

      mixmod_regression_return <- mixmod_regression_server(
        id = "mixmod_regression",
        .values = .values,
        estimate_cdf_r = probability_estimation_return$estimate_cdf_r
      )

      confint_betabinom_return <- confint_betabinom_server(
        id = "confint_betabinom",
        .values = .values,
        rank_regression_r = rank_regression_return$rank_regression_r,
        plot_prob_r = probability_estimation_return$plot_prob_r
      )

      confint_fisher_return <- confint_fisher_server(
        id = "confint_fisher",
        .values = .values,
        ml_estimation_r = ml_estimation_return$ml_estimation_r,
        plot_prob_r = probability_estimation_return$plot_prob_r
      )

      mcs_data_r <- reactive({
        data.frame(
          date_1 = c("2014-07-28", "2014-02-17", "2014-07-14",
                     "2014-06-26", "2014-03-10", "2014-05-14",
                     "2014-05-06", "2014-03-07", "2014-03-09",
                     "2014-04-13", "2014-05-20", "2014-07-07",
                     "2014-01-27", "2014-01-30", "2014-03-17",
                     "2014-02-09", "2014-04-14", "2014-04-20",
                     "2014-03-13", "2014-02-23", "2014-04-03",
                     "2014-01-08", "2014-01-08"),
          date_2 = c(NA, "2014-03-29", "2014-12-06", "2014-09-09",
                     NA, NA, "2014-06-16", NA, "2014-05-23",
                     "2014-05-09", "2014-05-31", NA, "2014-04-13",
                     NA, NA, "2014-03-12", NA, "2014-06-02",
                     NA, "2014-03-21", "2014-06-19", NA, NA),
          time = rep(1000, 23),
          status = c(0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0)
        )
      })

      mcs_delay_return <- mcs_delay_server(
        id = "mcs_delay",
        .values = .values,
        mcs_data_r = mcs_data_r
      )

      mcs_mileage_r <- reactive({
        data.frame(
          mileage = c(NA, 15655, 13629, 18292, NA, NA, 33555, NA, 21737,
                      29870, 21068, NA, 122283, NA, NA, 36088, NA, 11153,
                      NA, 122842, 20349, NA, NA),
          time = rep(1000, 23),
          status = c(0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0)
        )
      })

      mcs_mileage_return <- mcs_mileage_server(
        id = "mcs_mileage",
        .values = .values,
        mcs_data_r = mcs_mileage_r
      )
    }
  )
}
