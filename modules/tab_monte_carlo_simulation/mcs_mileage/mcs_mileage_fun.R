mcs_mileage_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "mcs_mileage",
    varname = "mcs_mileage_result",
    r_function_arg(
      "x",
      shiny::uiOutput(
        outputId = ns("x"),
        container = htmltools::pre
      )
    ),
    r_function_arg(
      name = "distribution",
      preSelectInput(
        inputId = ns("distribution"),
        label = NULL,
        choices = c("lognormal", "exponential")
      )
    )
  )
}

mcs_mileage_fun_server <- function(id, .values, mcs_mileage_data_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      rd_varname <- attr(mcs_mileage_data_r, "shinymetaVarname", exact = TRUE)

      output$x <- shiny::renderUI({
        varname_link(
          tabName = "mcs_mileage_data",
          varname = rd_varname
        )
      })

      mcs_mileage_r <- shinymeta::metaReactive({
        mcs_mileage(
          x = ..(mcs_mileage_data_r()),
          distribution = ..(shiny::req(input$distribution))
        )
      }, varname = "mcs_mileage_result")

      return_list <- list(
        mcs_mileage_r = mcs_mileage_r
      )

      return(return_list)
    }
  )
}
