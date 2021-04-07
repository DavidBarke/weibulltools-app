qf_incompatible_distributions_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::div(
    id = ns("container"),
    class = "btn-list"
  )
}

qf_incompatible_distributions_server <- function(id,
                                .values,
                                error_message_r,
                                model_distribution_id,
                                model_session
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      quick_fix_shown_rv <- shiny::reactiveVal(FALSE)

      is_incompatible_distribution_error_r <- shiny::reactive({
        !is.null(error_message_r()) &&
          stringr::str_detect(error_message_r(), "Incompatible distributions!")
      })

      # x[1]: distribution of plot_prob
      # x[2]: distribution of model
      # only evaluated when incompatible distribution error therefore no
      # additional check required
      distributions_r <- shiny::reactive({
        stringr::str_match_all(error_message_r(), "'([^']*)'")[[1]][,2]
      })

      shiny::observeEvent(error_message_r(), {
        if (!is_incompatible_distribution_error_r()) {
          quick_fix_shown_rv(FALSE)

          shiny::removeUI(
            selector = paste0("#", ns("container"), " *"),
            multiple = TRUE
          )
        } else {
          if (quick_fix_shown_rv()) return()

          quick_fix_shown_rv(TRUE)
          shiny::insertUI(
            selector = paste0("#", ns("container")),
            where = "afterBegin",
            ui = htmltools::tagList(
              shiny::actionButton(
                inputId = ns("quick_fix_plot_prob"),
                label = "Quick Fix: Update distribution of probability plot",
                icon = shiny::icon("wrench")
              ),
              shiny::actionButton(
                inputId = ns("quick_fix_model"),
                label = "Quick Fix: Update distribution of model",
                icon = shiny::icon("wrench")
              )
            )
          )
        }
      }, ignoreNULL = FALSE)

      shiny::observeEvent(input$quick_fix_model, {
        bs4Dash::toast(
          title = "Quick Fix",
          body = glue::glue(
            "Set distribution of model to \"{distribution}\"",
            distribution = distributions_r()[1]
          ),
          options = .values$toast_options()
        )

        shiny::updateSelectInput(
          inputId = model_distribution_id,
          session = model_session,
          selected = distributions_r()[1]
        )
      })

      shiny::observeEvent(input$quick_fix_plot_prob, {
        bs4Dash::toast(
          title = "Quick Fix",
          body = glue::glue(
            "Set distribution of probability plot to \"{distribution}\"",
            distribution = distributions_r()[2]
          ),
          options = .values$toast_options()
        )

        shiny::updateSelectInput(
          inputId = .values$plot_prob_distribution_id,
          session = .values$plot_prob_session,
          selected = std_distribution(distributions_r()[2])
        )
      })
    }
  )
}
