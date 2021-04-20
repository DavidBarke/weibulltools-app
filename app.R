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
        waiter::waiter_show_on_load(waiter::spin_wave()),
        #htmltools::includeScript("www/js/dark-mode.js"),
        htmltools::includeCSS("www/css/styles.css"),
        htmltools::includeCSS("www/css/dark.css"),
        htmltools::includeCSS("www/css/dt-dark.css"),
        htmltools::tags$link(
            rel = "stylesheet",
            href = "//cdnjs.cloudflare.com/ajax/libs/highlight.js/10.6.0/styles/default.min.css"
        ),
        htmltools::tags$script(
            src = "//cdnjs.cloudflare.com/ajax/libs/highlight.js/10.6.0/highlight.min.js"
        ),
        htmltools::tags$script(
            src="https://cdn.jsdelivr.net/npm/js-cookie@rc/dist/js.cookie.min.js"
        ),
        htmltools::tags$script("hljs.highlightAll();"),
        container_ui(
            id = "container"
        ),
        # Enable shinyjs
        useShinyjs(),
        # Enable rclipboard
        rclipboard::rclipboardSetup(),
        htmltools::tags$script(
            src="https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/1.7.1/clipboard.min.js",
            integrity="sha384-cV+rhyOuRHc9Ub/91rihWcGmMmCXDeksTtCihMupQHSsi8GIIRDG0ThDc3HGQFJ3",
            crossorigin="anonymous"
        ),
        # Extend shinyjs with custom JavaScript
        shinyjs::extendShinyjs(
            "min-js/cookies.js",
            functions = c("getCookie", "setCookie", "rmCookie")
        ),
        # Include minified script
        htmltools::includeScript("www/min-js/weibulltools-app.js")
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

        .values$toast_options <- function(...) {
            dots <- list(...)

            default <- list(
                position = "bottomRight",
                autohide = TRUE,
                delay = 3000,
                class = "bg-primary"
            )

            default %<-% dots
        }

        container_server(
            id = "container",
            .values = .values
        )

        # Hide waiter when initialisation is done
        waiter::waiter_hide()

        shiny::observeEvent(input$dark_mode, {
          .values$is_dark_mode_rv(input$dark_mode)
        })

        # Handle dark mode cookie
        shiny::observeEvent(TRUE, {
            js$getCookie(
                cookie = "dark-mode",
                id = "cookie_dark_mode"
            )
        }, once = TRUE)

        shiny::observeEvent(input$dark_mode, {
            js$setCookie(
                cookie = "dark-mode",
                value = input$dark_mode,
                id = "cookie_dark_mode"
            )
        })

        # Helper function defined in modules/ui/modal_reference.R
        .values$open_modal_reference <- open_modal_reference
        shiny::observeEvent(input$open_modal, {
            .values$open_modal_reference(input$open_modal$value)
        })
    }

    return(list(ui = ui, server = server))
}

ui_server <- ui_server(source_to_globalenv = FALSE)

ui <- ui_server$ui
server <- ui_server$server

shiny::shinyApp(ui, server)
