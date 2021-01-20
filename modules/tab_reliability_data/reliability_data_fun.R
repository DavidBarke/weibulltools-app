reliability_data_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  input_datasets <- c("alloy", "shock", "voltage")

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
      placeholder = shiny::uiOutput(
        outputId = ns("placeholder")
      ),
      r_function_arg(
        "data",
        preSelectInput(
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
        htmltools::pre("status")
      ),
      r_function_arg(
        "id",
        htmltools::pre("NULL")
      )
    )
  )
}

reliability_data_fun_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$placeholder <- shiny::renderUI({
        x <- paste(
          shiny::req(input$data),
          paste("x =", x_r()),
          "status = status",
          "id = NULL",
          sep = ", "
        )

        htmltools::pre(
          x
        )
      })

      shiny::outputOptions(
        output,
        "placeholder",
        suspendWhenHidden = FALSE
      )

      data_r <- shinymeta::metaReactive({
        get(..(shiny::req(input$data)), "package:weibulltools")
      }, varname = "data")

      x_dict_r <- shiny::reactive({
        c(
          "alloy" = "cycles",
          "shock" = "distance",
          "voltage" = "hours"
        )
      })

      x_r <- shiny::reactive({
        x_dict_r()[[shiny::req(input$data)]]
      })

      output$x <- shiny::renderUI({
        htmltools::pre(x_r())
      })

      reliability_data_r <- shinymeta::metaReactive({
        reliability_data(
          data = ..(data_r()),
          # needs weibulltools 2.0.0:9000
          x = ..(x_r()),
          status = "status",
          id = NULL
        )
      }, varname = "reliability_data")

      return_list <- list(
        reliability_data_r = reliability_data_r,
        data_r = shiny::reactive(shiny::req(input$data))
      )

      return(return_list)
    }
  )
}
