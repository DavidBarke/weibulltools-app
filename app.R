library(shiny)
library(shinyjs)
library(shinycssloaders)
library(bs4Dash)
library(dplyr)
library(purrr)
library(DT)
library(R.utils)

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
        tags$head(
            # Include custom css styles
            # shiny::includeCSS("www/css/styles.css")
        ),
        container_ui(
            id = "container"
        ),
        # Enable shinyjs
        useShinyjs()
    )

    # SERVER -------------------------------------------------------------------

    server <- function(input, output, session) {

        # .VALUES ENVIRONMENT ------------------------------------------------

        # The .values environment is available to all modules so that arbitrary information
        # can be shared via this environment. Elements that underly reactive changes can be
        # stored as reactiveValues or reactiveVal
        .values <- new.env()

        container_server(
            id = "container",
            .values = .values
        )

        # session$onSessionEnded(function() {
        # })
    }

    return(list(ui = ui, server = server))
}

ui_server <- ui_server(source_to_globalenv = FALSE)

ui <- ui_server$ui
server <- ui_server$server

shiny::shinyApp(ui, server)
