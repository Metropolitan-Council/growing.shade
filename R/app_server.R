#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  callModule(mod_intro_server, "intro_ui_1")
  callModule(mod_stories_server, "stories_ui_1")
  callModule(mod_story_generator_server, "story_generator_ui_1")
  callModule(mod_where_server, "where_ui_1")
  callModule(mod_next_server, "nexte_ui_1")
  mod_storymap_server("storymap_ui_1")
  
  map_selections <- callModule(mod_map_selections_server, "map_selections_ui_1")
  
  # observe({print(map_selections$allInputs)}) #to check that selections are working
  
  map_util <- callModule(mod_map_utils_server, "map_utils_ui_1",
                         map_selections = map_selections)
  
  # observe({print((map_util$map_data2))}) #to check that data summary is working
  # observe({print((map_util$plot_data2))}) #to check that plot summary is working
  
  tract_selections <- callModule(mod_map_overview_server, "map_overview_ui_1",
             map_selections = map_selections,
             map_util = map_util,
             current_tab = input$nav)
  
  # callModule(
  #   mod_main_leaflet_server,
  #   "main_leaflet_ui_1",
  #   map_util,
  #   map_selections,
  #   current_tab = input$nav
  # )
  
  observe({print(tract_selections$selected_tract)}) #to check that tract clicking is working
  
  callModule(mod_ndvi_map_server, "ndvi_map_ui_1",
             tract_selections = tract_selections)
  
  callModule(mod_plot_tract_server, "plot_tract_ui_1",
             tract_selections = tract_selections,
             map_util = map_util)
  
  callModule(mod_table_server, "table_ui_1",
             tract_selections = tract_selections,
             map_util = map_util)
  
  callModule(mod_biodiversity_server, "biodiversity_ui_1")
  
  callModule(mod_next_server, "next_ui_1")
  
  
  }
