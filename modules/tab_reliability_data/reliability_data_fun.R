reliability_data_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  input_datasets <- c("shock", "alloy", "voltage")

  r_function(
    name = "reliability_data",
    varname = ref_dropdown(
      id = ns("ref_dropdown"),
      varname = r_function_varname("rel_tbl"),
      ref_tbl = tibble::tibble(
        label = c("estimate_cdf", "ml_estimation", "mixmod_em"),
        reference = c("estimate_cdf", "ml_estimation", "mixmod_em"),
        tabName = c("probability_estimation", "ml_estimation", "mixmod_em")
      )
    ),
    placeholder = shiny::uiOutput(
      outputId = ns("placeholder"),
      container = htmltools::pre
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
}

reliability_data_fun_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$placeholder <- shiny::renderUI({
        paste(
          input$data %||% "shock",
          paste("x =", x_r()),
          "status = status",
          "id = NULL",
          sep = ", "
        )
      })

      shiny::outputOptions(
        output,
        "placeholder",
        suspendWhenHidden = FALSE
      )

      data_r <- shinymeta::metaReactive({
        get(..(input$data %||% "shock"), "package:weibulltools")
      }, varname = "data")

      x_dict_r <- shiny::reactive({
        c(
          "alloy" = "cycles",
          "shock" = "distance",
          "voltage" = "hours"
        )
      })

      x_r <- shiny::reactive({
        x_dict_r()[[input$data %||% "shock"]]
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
      }, varname = "rel_tbl")

      return_list <- list(
        reliability_data_r = reliability_data_r,
        data_r = shiny::reactive(input$data %||% "shock")
      )

      return(return_list)
    }
  )
}
