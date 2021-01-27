mcs_delay_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "mcs_delay",
    varname = "mcs_d",
    r_function_arg(
      "date_1"
    ),
    r_function_arg(
      "date_2"
    ),
    r_function_arg(
      "time"
    ),
    r_function_arg(
      "status",
      htmltools::pre("NULL")
    ),
    r_function_arg(
      "id",
      htmltools::pre('paste0("ID", seq_len(length(time)))')
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

mcs_delay_fun_server <- function(id, .values, mcs_data_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      # rd_varname <- attr(reliability_data_r, "shinymetaVarname", exact = TRUE)
      #
      # output$x <- shiny::renderUI({
      #   varname_link_ui(
      #     id = ns("varname_link_reliability_data"),
      #     varname = rd_varname
      #   )
      # })
      #
      # varname_link_server(
      #   id = "varname_link_reliability_data",
      #   .values = .values,
      #   tabName = "reliability_data",
      #   varname = rd_varname
      # )

      mcs_delay_r <- shinymeta::metaReactive({
        mcs_delay(
          date_1 = ..(mcs_data_r()$date_1),
          date_2 = ..(mcs_data_r()$date_2),
          time = ..(mcs_data_r()$time),
          status = ..(mcs_data_r()$status),
          distribution = ..(shiny::req(input$distribution))
        )
      }, varname = "mcs_d")

      return_list <- list(
        mcs_delay_r = mcs_delay_r
      )

      return(return_list)
    }
  )
}
