#' other_resources UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_other_resources_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # br(class="d-none d-lg-block"), br(class="d-none d-lg-block"),
    br(), br(),
    shiny::div(
      id = "otherresources",
      # includeMarkdown(paste0(here::here(),("inst/app/www/otherresources.md")))

      includeMarkdown(system.file("app/www/otherresources.md", package = "planting.shade"))
      # includeMarkdown(system.file("inst/app/www/otherresources.md", package = "planting.shade"))
    ),
    br()
  )
}

#' other_resources Server Functions
#'
#' @noRd
mod_other_resources_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_other_resources_ui("other_resources_ui_1")

## To be copied in the server
# mod_other_resources_server("other_resources_ui_1")
