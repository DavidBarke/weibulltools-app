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
    placeholder = shiny::uiOutput(
      outputId = ns("placeholder"),
      container = htmltools::pre
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

      date_1_r <- shiny::reactive({
        input$date_1 %||% "production_date"
      })

      date_2_r <- shiny::reactive({
        input$date_2 %||% "registration_date"
      })

      output$placeholder <- shiny::renderUI({
        glue::glue(
          '
          data = field_data,
          date_1 = {date_2},
          date_2 = {date_1},
          time = "dis",
          status = "status",
          id = NULL
          ',
          date_1 = format_vector(date_1_r()),
          date_2 = format_vector(date_2_r())
        )
      })

      shiny::outputOptions(
        output,
        "placeholder",
        suspendWhenHidden = FALSE
      )

      data_r <- shinymeta::metaReactive({
        get(..("field_data"), "package:weibulltools")
      }, varname = "data")

      mcs_delay_data_r <- shinymeta::metaReactive({
        mcs_delay_data(
          data = ..(data_r()),
          date_1 = ..(date_1_r()),
          date_2 = ..(date_2_r()),
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
