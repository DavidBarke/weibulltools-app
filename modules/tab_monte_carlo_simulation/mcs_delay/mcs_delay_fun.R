mcs_delay_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "mcs_delay",
    varname = "mcs_delay_result",
    placeholder = shiny::uiOutput(
      outputId = ns("placeholder"),
      container = htmltools::pre
    ),
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

mcs_delay_fun_server <- function(id, .values, mcs_delay_data_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      rd_varname <- attr(mcs_delay_data_r, "shinymetaVarname", exact = TRUE)

      output$placeholder <- shiny::renderUI({
        glue::glue(
          '
          x = {x},
          distribution = "{distribution}"
          ',
          x = rd_varname,
          distribution = input$distribution
        )
      })

      shiny::outputOptions(
        output,
        "placeholder",
        suspendWhenHidden = FALSE
      )

      output$x <- shiny::renderUI({
        varname_link(
          tabName = "mcs_delay_data",
          varname = rd_varname
        )
      })

      mcs_delay_r <- shinymeta::metaReactive({
        mcs_delay(
          x = ..(mcs_delay_data_r()),
          distribution = ..(input$distribution)
        )
      }, varname = "mcs_delay_result")

      return_list <- list(
        mcs_delay_r = mcs_delay_r
      )

      return(return_list)
    }
  )
}
