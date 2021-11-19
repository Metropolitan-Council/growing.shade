#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  shinyhelper::observe_helpers(help_dir = "inst/app/www")
  options(shiny.usecairo=T)
  
  
  callModule(mod_intro_server, "intro_ui_1")
  mod_storymap_server("storymap_ui_1")
  
  # observeEvent(input$link_to_tabpanel_b, {
  #   updateTabsetPanel(session, "demo", "B")
  # })
  

  
  # preset_selections <- mod_preset_selections_server("preset_selections_ui_1")#,
  #                                                   # current_tab = input$nav)
  
  # observe({print(paste0("preset selection: ", preset_selections$preset))})

  map_selections <- callModule(mod_map_selections_server, "map_selections_ui_1",
                               # preset_selections,
                               current_tab = input$nav)
  
  geo_selections <- mod_geo_selection_server("geo_selection_ui_1")
  
  observe({print(paste0("which geography mapping: ", (geo_selections$selected_geo)))})
  observe({print(paste0("selected ctu/neighborhood from dropdown: ", (geo_selections$selected_area)))})
  
  # observe({print(paste0("is the priority layer on/off: ", map_selections$priority_layer))})
  observe({print(paste0("preset used: ", map_selections$preset))}) #to check that selections are working
  observe({print(paste0("variables used: ", map_selections$allInputs))}) #to check that selections are working

  map_util <- callModule(mod_map_utils_server, "map_utils_ui_1",
                         map_selections = map_selections,
                         geo_selections = geo_selections)
  
  # observe({print((map_util$map_data2))}) #to check that data summary is working
  # observe({print((map_util$plot_data2))}) #to check that plot summary is working
  
  tract_selections <- callModule(mod_map_overview_server, "map_overview_ui_1",
                                 geo_selections = geo_selections,
             map_selections = map_selections,
             map_util = map_util,
             current_tab = input$nav)
  # observe({print(paste0("is.null event$id: ", is.null(tract_selections$TEST)))}) 
  observe({print(paste0("selected tract: ", (tract_selections$selected_tract)))}) #to check that selections are working
  
  mod_report_server("report_ui_1",
                    geo_selections,
                    map_selections,
                    tract_selections,
                    map_util)
  
  # observe({print(paste0("selected tract: ", (tract_selections$clicked_geo)))}) #to check that selections are working
  
    # callModule(
  #   mod_main_leaflet_server,
  #   "main_leaflet_ui_1",
  #   map_util,
  #   map_selections,
  #   current_tab = input$nav
  # )
  
  # callModule(mod_ndvi_map_server, "ndvi_map_ui_1",
  #            tract_selections = tract_selections)
  # 
  # callModule(mod_plot_tract_server, "plot_tract_ui_1",
  #            tract_selections = tract_selections,
  #            map_util = map_util,
  #            map_selections = map_selections)
  # 
  # callModule(mod_table_server, "table_ui_1",
  #            tract_selections = tract_selections,
  #            map_util = map_util)
  # 
  # callModule(mod_biodiversity_server, "biodiversity_ui_1")
  # 
  # callModule(mod_next_server, "next_ui_1")
  
  
  }
