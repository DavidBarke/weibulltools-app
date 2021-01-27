mcs_mileage_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "mcs_mileage",
    varname = "mcs_miles",
    r_function_arg(
      "mileage"
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

mcs_mileage_fun_server <- function(id, .values, mcs_data_r) {
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

      mcs_mileage_r <- shinymeta::metaReactive({
        mcs_mileage(
          mileage = ..(mcs_data_r()$mileage),
          time = ..(mcs_data_r()$time),
          status = ..(mcs_data_r()$status),
          distribution = ..(shiny::req(input$distribution))
        )
      }, varname = "mcs_miles")

      return_list <- list(
        mcs_mileage_r = mcs_mileage_r
      )

      return(return_list)
    }
  )
}
