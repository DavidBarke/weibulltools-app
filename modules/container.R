container_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::bs4DashPage(
    header = bs4Dash::bs4DashNavbar(
      title = "weibulltools"
    ),
    sidebar = bs4Dash::bs4DashSidebar(
      id = ns("sidebar"),
      width = "290px",
      bs4Dash::sidebarMenu(
        bs4Dash::menuItem(
          text = "Overview",
          tabName = "overview"
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
          tabName = "parameter_estimation"
        ),
        bs4Dash::menuItem(
          text = "Confidence Intervals",
          tabName = "confidence_intervals"
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
          tabName = "parameter_estimation",
          parameter_estimation_ui(
            id = ns("parameter_estimation")
          )
        ),
        bs4Dash::bs4TabItem(
          tabName = "confidence_intervals",
          confidence_intervals_ui(
            id = ns("confidence_intervals")
          )
        )
      )
    ),
    title = "DashboardPage"
  )
}

container_server <- function(id, .values) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

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

      reliability_data_server(
        id = "reliability_data",
        .values = .values
      )

      probability_estimation_server(
        id = "probability_estimation",
        .values = .values
      )

      parameter_estimation_server(
        id = "parameter_estimation",
        .values = .values
      )

      confidence_intervals_server(
        id = "confidence_intervals",
        .values = .values
      )

    }
  )
}
