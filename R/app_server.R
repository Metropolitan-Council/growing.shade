#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here
  # shinyhelper::observe_helpers(help_dir = "inst/app/www")
  # options(shiny.usecairo = T)

  # waitress <- waiter::Waitress$new("nav", theme = "overlay", min = 0, max = 10)

  mod_storymap_server("storymap_ui_1")
  mod_faq_server("faq_ui_1")
  # observe({print(paste0("preset selection: ", preset_selections$preset))})

  
  map_selections <- callModule(
    mod_map_selections_server, "map_selections_ui_1" # ,
    # current_tab = input$nav
  )

  geo_selections <- mod_geo_selection_server("geo_selection_ui_1")

  # observe({
  #   print(paste0("which geography mapping: ", (geo_selections$selected_geo)))
  # })
  # observe({
  #   print(paste0("selected ctu/neighborhood from dropdown: ", (geo_selections$selected_area)))
  # })
  #
  # observe({
  #   print(paste0("preset used: ", map_selections$preset))
  # }) # to check that selections are working
  # observe({
  #   print(paste0("variables used: ", map_selections$allInputs))
  # }) # to check that selections are working

  map_util <- callModule(mod_map_utils_server, "map_utils_ui_1",
    map_selections = map_selections,
    geo_selections = geo_selections
  )

#   # browser()
#   observe({print(head(map_util$map_data2))}) #to check that data summary is working
#   observe({print(head(map_util$map_data))}) #to check that plot summary is working
# browser()
  blockgroup_selections <- callModule(mod_map_overview_server, "map_overview_ui_1",
    geo_selections = geo_selections,
    map_selections = map_selections,
    map_util = map_util,
    current_tab = input$nav
  )
  #
  # observe({
  #   print(paste0("selected blockgroup: ", (blockgroup_selections$selected_blockgroup)))
  # }) # to check that selections are working

  mod_report_server(
    "report_ui_1",
    geo_selections,
    map_selections,
    blockgroup_selections,
    map_util
  )
}
