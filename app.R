library(shiny)
library(shinymeta)
library(shinyjs)
library(shinycssloaders)
library(bs4Dash)
library(dplyr)
library(purrr)
library(DT)
library(R.utils)
library(weibulltools)
library(xml2)

reactlog::reactlog_enable()

shiny::addResourcePath("articles", "./articles")

ui_server <- function(source_to_globalenv = FALSE) {
    # If source_to_global_env all sourced functions get added to the global
    # environment which takes some time after the app has stopped

    source("init/source_directory.R")

    source_directory(
        # chdir enables use of relative paths in source statements inside
        # these sourced files
        path = "./modules",
        encoding = "UTF-8",
        modifiedOnly = FALSE,
        chdir = TRUE,
        recursive = TRUE,
        envir = if (source_to_globalenv) globalenv() else environment()
    )

    # Globals ------------------------------------------------------------------

    # Allow bigger file inputs
    options(shiny.maxRequestSize = 100*1024^2)

    # modules/dt_options.R
    dt_options()

    # UI -----------------------------------------------------------------------
    ui <- htmltools::div(
        waiter::use_waiter(),
        waiter::waiter_show_on_load(waiter::spin_solar()),
        htmltools::includeScript("www/js/dark-mode.js"),
        tags$head(
            # Include custom css styles
            htmltools::includeCSS("www/css/styles.css"),
            htmltools::includeCSS("www/css/dark.css"),
            htmltools::includeCSS("www/css/dt-dark.css")
        ),
        container_ui(
            id = "container"
        ),
        # Enable shinyjs
        useShinyjs(),
        # Enable rclipboard
        rclipboard::rclipboardSetup(),
        # Include custom scripts
        htmltools::includeScript("www/js/up-down-btn.js"),
        htmltools::includeScript("www/js/init-popover.js"),
        htmltools::includeScript("www/js/emphasize.js"),
        shinyjs::extendShinyjs(
            "js/extend-shinyjs.js", functions = "bindResizeIframe"
        )
    )

    # SERVER -------------------------------------------------------------------

    server <- function(input, output, session) {

        # .VALUES ENVIRONMENT ------------------------------------------------

        # The .values environment is available to all modules so that arbitrary information
        # can be shared via this environment. Elements that underly reactive changes can be
        # stored as reactiveValues or reactiveVal
        .values <- new.env()

        .values$code_header <- quote(library(weibulltools))
        .values$is_dark_mode_rv <- shiny::reactiveVal(FALSE)

        container_server(
            id = "container",
            .values = .values
        )

        waiter::waiter_hide()

        shiny::observeEvent(input$dark_mode, {
          .values$is_dark_mode_rv(input$dark_mode)
        })

        # session$onSessionEnded(function() {
        # })
    }

    return(list(ui = ui, server = server))
}

ui_server <- ui_server(source_to_globalenv = FALSE)

ui <- ui_server$ui
server <- ui_server$server

shiny::shinyApp(ui, server)
