container_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::bs4DashPage(
    header = bs4Dash::bs4DashNavbar(
      title = bs4Dash::bs4DashBrand(
        title = "weibulltools",
        href = "https://github.com/Tim-TU/weibulltools/"
      ),
      rightUi = htmltools::tagList(
        htmltools::tags$li(
          # Fake dropdown
          class = "dropdown",
          htmltools::a(
            class = "github-link",
            href = "https://github.com/Tim-TU/weibulltools",
            shiny::icon("github")
          )
        )
      )
    ),
    sidebar = bs4Dash::bs4DashSidebar(
      width = "290px",
      skin = "light",
      bs4Dash::sidebarMenu(
        id = ns("sidebar"),
        bs4Dash::menuItem(
          text = "Getting Started",
          tabName = "getting_started"
        ),
        bs4Dash::menuItem(
          text = "Comprehensive Example",
          tabName = "example"
        ),
        bs4Dash::menuItem(
          text = "Datasets",
          bs4Dash::menuSubItem(
            text = "alloy",
            tabName = "alloy"
          ),
          bs4Dash::menuSubItem(
            text = "shock",
            tabName = "shock"
          ),
          bs4Dash::menuSubItem(
            text = "voltage",
            tabName = "voltage"
          ),
          bs4Dash::menuSubItem(
            text = "field_data",
            tabName = "field_data"
          )
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
          tabName = "getting_started",
          getting_started_ui(
            id = ns("getting_started")
          )
        ),
        bs4Dash::bs4TabItem(
          tabName = "example",
          example_ui(
            id = ns("example")
          )
        ),
        bs4Dash::bs4TabItem(
          tabName = "alloy",
          dataset_ui(
            id = ns("alloy"),
            ref_name = "alloy"
          )
        ),
        bs4Dash::bs4TabItem(
          tabName = "shock",
          dataset_ui(
            id = ns("shock"),
            ref_name = "shock"
          )
        ),
        bs4Dash::bs4TabItem(
          tabName = "voltage",
          dataset_ui(
            id = ns("voltage"),
            ref_name = "voltage"
          )
        ),
        bs4Dash::bs4TabItem(
          tabName = "field_data",
          dataset_ui(
            id = ns("field_data"),
            ref_name = "field_data"
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

      dependencies <- list(
        getting_started = character(),
        example = character(),
        alloy = character(),
        shock = character(),
        voltage = character(),
        field_data = character(),
        reliability_data = character(),
        mcs_delay_data = character(),
        mcs_mileage_data = character(),
        probability_estimation = "reliability_data",
        ml_estimation = "probability_estimation",
        rank_regression = "probability_estimation",
        mixmod_em = "reliability_data",
        mixmod_regression = "probability_estimation",
        confint_betabinom = "rank_regression",
        confint_fisher = "ml_estimation",
        mcs_delay = "mcs_delay_data",
        mcs_mileage = "mcs_mileage_data"
      )

      servers <- list(
        getting_started = function() {
          getting_started_server(
            id = "getting_started",
            .values = .values
          )
        },
        example = function() {
          example_server(
            id = "example",
            .values = .values
          )
        },
        alloy = function() {
          dataset_server(
            id = "alloy",
            .values = .values,
            dataset = "alloy"
          )
        },
        shock = function() {
          dataset_server(
            id = "shock",
            .values = .values,
            dataset = "shock"
          )
        },
        voltage = function() {
          dataset_server(
            id = "voltage",
            .values = .values,
            dataset = "voltage"
          )
        },
        field_data = function() {
          dataset_server(
            id = "field_data",
            .values = .values,
            dataset = "field_data"
          )
        },
        reliability_data = function() {
          reliability_data_return <<- reliability_data_server(
            id = "reliability_data",
            .values = .values
          )
        },
        mcs_delay_data = function() {
          mcs_delay_data_return <<- mcs_delay_data_server(
            id = "mcs_delay_data",
            .values = .values
          )
        },
        mcs_mileage_data = function() {
          mcs_mileage_data_return <<- mcs_mileage_data_server(
            id = "mcs_mileage_data",
            .values = .values
          )
        },
        probability_estimation = function() {
          probability_estimation_return <<- probability_estimation_server(
            id = "probability_estimation",
            .values = .values,
            reliability_data_r = reliability_data_return$reliability_data_r
          )
        },
        ml_estimation = function() {
          ml_estimation_return <<- ml_estimation_server(
            id = "ml_estimation",
            .values = .values,
            reliability_data_r = reliability_data_return$reliability_data_r,
            plot_prob_r = probability_estimation_return$plot_prob_r
          )
        },
        rank_regression = function() {
          rank_regression_return <<- rank_regression_server(
            id = "rank_regression",
            .values = .values,
            estimate_cdf_r = probability_estimation_return$estimate_cdf_r,
            plot_prob_r = probability_estimation_return$plot_prob_r
          )
        },
        mixmod_em = function() {
          mixmod_em_return <<- mixmod_em_server(
            id = "mixmod_em",
            .values = .values,
            reliability_data_r = reliability_data_return$reliability_data_r
          )
        },
        mixmod_regression = function() {
          mixmod_regression_return <<- mixmod_regression_server(
            id = "mixmod_regression",
            .values = .values,
            estimate_cdf_r = probability_estimation_return$estimate_cdf_r
          )
        },
        confint_betabinom = function() {
          confint_betabinom_return <<- confint_betabinom_server(
            id = "confint_betabinom",
            .values = .values,
            rank_regression_r = rank_regression_return$rank_regression_r,
            plot_prob_r = probability_estimation_return$plot_prob_r
          )
        },
        confint_fisher = function() {
          confint_fisher_return <<- confint_fisher_server(
            id = "confint_fisher",
            .values = .values,
            ml_estimation_r = ml_estimation_return$ml_estimation_r,
            plot_prob_r = probability_estimation_return$plot_prob_r
          )
        },
        mcs_delay = function() {
          mcs_delay_return <<- mcs_delay_server(
            id = "mcs_delay",
            .values = .values,
            mcs_delay_data_r = mcs_delay_data_return$mcs_delay_data_r
          )
        },
        mcs_mileage = function() {
          mcs_mileage_return <<- mcs_mileage_server(
            id = "mcs_mileage",
            .values = .values,
            mcs_mileage_data_r = mcs_mileage_data_return$mcs_mileage_data_r
          )
        }
      )

      called_rv <- shiny::reactiveVal(character())

      # Don't call all modules on app start. Instead wait until tab is opened
      shiny::observeEvent(input$sidebar, {
        call_modules(
          id = input$sidebar,
          dependencies = dependencies,
          servers = servers,
          called_rv = called_rv
        )
      })
    }
  )
}



call_modules <- function(id, dependencies, servers, called_rv) {
  deps <- dependencies[[id]]

  # Call dependencies
  purrr::walk(deps, function(dep) {
    if (!dep %in% called_rv()) {
      # Recursive step
      call_modules(dep, dependencies, servers, called_rv)
    }
  })

  if (!id %in% called_rv()) {
    called_rv(c(called_rv(), id))
    # Call server function of this module
    servers[[id]]()
  }
}
