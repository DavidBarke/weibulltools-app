mcs_mileage_data_fun_ui <- function(id) {
  ns <- shiny::NS(id)

  r_function(
    name = "mcs_mileage_data",
    varname = ref_dropdown(
      id = ns("ref_dropdown"),
      varname = r_function_varname("mcs_mileage_tbl"),
      ref_tbl = tibble::tibble(
        label = "mcs_mileage",
        reference = "mcs_mileage",
        tabName = "mcs_mileage"
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
      "mileage",
      htmltools::pre('mileage')
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

mcs_mileage_data_fun_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$placeholder <- shiny::renderUI({
        glue::glue(
          '
          data = field_data,
          mileage = "mileage",
          time = "dis",
          status = "status",
          id = NULL
          '
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

      mcs_mileage_data_r <- shinymeta::metaReactive({
        mcs_mileage_data(
          data = ..(data_r()),
          mileage = "mileage",
          time = "dis",
          status = "status",
          id = NULL
        )
      }, varname = "mcs_mileage_tbl")

      return_list <- list(
        mcs_mileage_data_r = mcs_mileage_data_r
      )

      return(return_list)
    }
  )
}
