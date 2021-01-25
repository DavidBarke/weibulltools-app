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

    }
  )
}
