#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here

  mod_storymap_server("storymap_ui_1")
  mod_faq_server("faq_ui_1")


  map_selections <- callModule(
    mod_map_selections_server, "map_selections_ui_1"
  )

  geo_selections <- mod_geo_selection_server("geo_selection_ui_1")

  map_util <- callModule(mod_map_utils_server, "map_utils_ui_1",
    map_selections = map_selections,
    geo_selections = geo_selections
  )

  blockgroup_selections <- callModule(mod_map_overview_server, "map_overview_ui_1",
    geo_selections = geo_selections,
    map_selections = map_selections,
    map_util = map_util,
    current_tab = input$nav
  )

  mod_report_server(
    "report_ui_1",
    geo_selections,
    map_selections,
    blockgroup_selections,
    map_util
  )
}
