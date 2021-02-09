mcs_delay_data_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "mcs_delay_data",
    varname = ref_dropdown(
      id = ns("ref_dropdown"),
      varname = r_function_varname("mcs_delay_tbl"),
      ref_tbl = tibble::tibble(
        label = "mcs_delay",
        reference = "mcs_delay",
        tabName = "mcs_delay"
      )
    ),
    r_function_arg(
      "data",
      htmltools::pre('field_data')
    ),
    r_function_arg(
      name = "date_1",
      preSelectInput(
        inputId = ns("date_1"),
        label = NULL,
        choices = c("production_date", "repair_date"),
        selected = "production_date",
        multiple = TRUE
      )
    ),
    r_function_arg(
      name = "date_2",
      preSelectInput(
        inputId = ns("date_2"),
        label = NULL,
        choices = c("registration_date", "report_date"),
        selected = "registration_date",
        multiple = TRUE
      )
    ),
    r_function_arg(
      "time",
      htmltools::pre("dis")
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

mcs_delay_data_fun_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      data_r <- shinymeta::metaReactive({
        get(..("field_data"), "package:weibulltools")
      }, varname = "data")

      mcs_delay_data_r <- shinymeta::metaReactive({
        mcs_delay_data(
          data = ..(data_r()),
          date_1 = ..(input$date_1 %||% "production_date"),
          date_2 = ..(input$date_2 %||% "registration_date"),
          time = "dis",
          status = "status",
          id = NULL
        )
      }, varname = "mcs_delay_tbl")

      return_list <- list(
        mcs_delay_data_r = mcs_delay_data_r
      )

      return(return_list)
    }
  )
}
