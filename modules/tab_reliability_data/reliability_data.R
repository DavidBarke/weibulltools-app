reliability_data_ui <- function(id) {
  ns <- shiny::NS(id)

  input_datasets <- c("alloy", "shock", "voltage")

  shiny::fluidRow(
    shiny::column(
      width = 6,
      bs4Dash::box(
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        title = "Datasets",
        shiny::selectInput(
          inputId = ns("input_data"),
          label = NULL,
          choices = input_datasets
        ),
        DT::dataTableOutput(
          outputId = ns("input_data_table")
        )
      ),
      bs4Dash::box(
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        title = "Reliability Data",
        htmltools::p(
          "Create consistent reliability data based on an existing data.frame."
        ),
        r_function(
          name = "reliability_data",
          r_function_arg(
            "data",
            shiny::selectInput(
              inputId = ns("data"),
              label = NULL,
              choices = input_datasets
            )
          ),
          r_function_arg(
            "x",
            shiny::uiOutput(
              outputId = ns("x")
            )
          ),
          r_function_arg(
            "status",
            shiny::uiOutput(
              outputId = ns("status")
            )
          ),
          r_function_arg(
            "id",
            htmltools::pre("NULL")
          )
        )
      ),
      bs4Dash::box(
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        title = "Result",
        DT::dataTableOutput(
          outputId = ns("result")
        )
      )
    ),
    shiny::column(
      width = 6
    )
  )
}

reliability_data_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      input_data_r <- shiny::reactive({
        get(input$input_data)
      })

      output$input_data_table <- DT::renderDataTable({
        DT::datatable(input_data_r())
      })

      data_r <- shiny::reactive({
        get(input$data)
      })

      col_names_r <- shiny::reactive({
        names(data_r())
      })

      x_dict <- c(
        "alloy" = "cycles",
        "shock" = "distance",
        "voltage" = "hours"
      )

      output$x <- shiny::renderUI({
        x_dict[input$data]
      })

      output$status <- shiny::renderUI({
        "status"
      })

      reliability_data_r <- shiny::reactive({
        do.call(
          what = weibulltools::reliability_data,
          args = list(
            data = data_r(),
            x = shiny::req(input$x),
            status = shiny::req(input$status),
            id = NULL
          )
        )
      })

      output$result <- DT::renderDataTable({
        DT::datatable(reliability_data_r())
      })
    }
  )
}
