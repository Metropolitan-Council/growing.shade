#' faq UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_faq_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(), br(),
    shiny::div(
      id = "faq",
      includeMarkdown(system.file("inst/app/www/faq.md", package = "planting.shade"))
    ),
    br()
  )
}

#' faq Server Functions
#'
#' @noRd
mod_faq_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_faq_ui("faq_ui_1")

## To be copied in the server
# mod_faq_server("faq_ui_1")
