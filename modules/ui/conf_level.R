conf_level_ui <- function(id) {
  ns <- shiny::NS(id)

  r_conf_level_arg(
    inputId = ns("conf_level")
  )
}

conf_level_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      conf_level_r <- shiny::reactive({
        if (is.null(input$conf_level) || is.na(input$conf_level)) return(0.95)

        max(0, min(1, input$conf_level))
      })

      return_list <- list(
        conf_level_r = conf_level_r
      )

      return(return_list)
    }
  )
}
