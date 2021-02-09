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
          text = "MCS Data",
          bs4Dash::menuSubItem(
            text = "MCS Delay Data",
            tabName = "mcs_delay_data"
          ),
          bs4Dash::menuSubItem(
            text = "MCS Mileage Data",
            tabName = "mcs_mileage_data"
          )
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
          tabName = "mcs_delay_data",
          mcs_delay_data_ui(
            id = ns("mcs_delay_data")
          )
        ),
        bs4Dash::bs4TabItem(
          tabName = "mcs_mileage_data",
          mcs_mileage_data_ui(
            id = ns("mcs_mileage_data")
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

      mcs_delay_data_return <- mcs_delay_data_server(
        id = "mcs_delay_data",
        .values = .values
      )

      mcs_mileage_data_return <- mcs_mileage_data_server(
        id = "mcs_mileage_data",
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

      mcs_delay_return <- mcs_delay_server(
        id = "mcs_delay",
        .values = .values,
        mcs_delay_data_r = mcs_delay_data_return$mcs_delay_data_r
      )

      mcs_mileage_return <- mcs_mileage_server(
        id = "mcs_mileage",
        .values = .values,
        mcs_mileage_data_r = mcs_mileage_data_return$mcs_mileage_data_r
      )
    }
  )
}
