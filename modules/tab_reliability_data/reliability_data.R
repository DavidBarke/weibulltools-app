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
              choices = input_datasets,
              width = "100%"
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
            "status"
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
      width = 6,
      bs4Dash::box(
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        title = "Datasets",
        shiny::selectInput(
          inputId = ns("input_data"),
          label = NULL,
          choices = input_datasets,
          width = "100%"
        ),
        DT::dataTableOutput(
          outputId = ns("input_data_table")
        )
      ),
      bs4Dash::box(
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        title = "Code",
        shiny::verbatimTextOutput(
          outputId = ns("code")
        )
      )
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

      data_r <- shinymeta::metaReactive({
        get(..(input$data), "package:weibulltools")
      }, varname = "data")

      x_dict_r <- shinymeta::metaReactive({
        c(
          "alloy" = "cycles",
          "shock" = "distance",
          "voltage" = "hours"
        )
      }, varname = "x_dict")

      x_r <- shinymeta::metaReactive({
        ..(x_dict_r())[[..(input$data)]]
      }, varname = "x")

      output$x <- metaRender(shiny::renderUI, {
        ..(x_r())
      })

      reliability_data_r <- shinymeta::metaReactive({
        reliability_data(
          data = ..(data_r()),
          x = !!..(x_r()),
          status = "status",
          id = NULL
        )
      }, varname = "reliability_data")

      output$result <- DT::renderDataTable({
        DT::datatable(reliability_data_r())
      })

      output$code <- shiny::renderPrint({
        expandChain(
          quote(library(weibulltools)),
          reliability_data_r(),
          .expansionContext = shinymeta::newExpansionContext(ns = FALSE)
        )
      })
    }
  )
}
