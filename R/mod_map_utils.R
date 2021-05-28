#' map_utils UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_map_utils_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' map_utils Server Function
#'
#' @noRd 
mod_map_utils_server <- function(input, output, session,
                                 map_selections){
  ns <- session$ns
 
  #we need to make this data for a bar plot, or something like that
  make_plot_data2 <- reactive({
    p <- eva_data_main %>% 
      filter(name %in% map_selections$allInputs$value)
    return(p)
  })
  
  #but we want to get a single averaged value for every tract to put on the map
  make_map_data2 <- reactive({
    p <- eva_data_main %>%
      filter(name %in% map_selections$allInputs$value) %>%
      group_by(tract_string) %>%
      summarise(MEAN = mean(weights_scaled, na.rm = T)) %>%
      left_join(eva_tract_geometry, by = c("tract_string" = "GEOID")) %>%
      st_as_sf() %>%
      st_transform(4326) %>%
      mutate(RANK = min_rank(desc(MEAN)))
 
    # leaflet() %>%
    #   setView(lat = 44.963, lng = -93.22, zoom = 9) %>%
    #   addMapPane(name = "Stamen Toner", zIndex = 430) %>%
    #   addPolygons(data = p)

    return(p)
  })
  
  ##-------------
  
  vals <- reactiveValues()
  
  observe({
    vals$plot_data2 <- make_plot_data2()
  })
  
  observe({
    vals$map_data2 <- make_map_data2()
  })
  
  return(vals)
  
}
    
## To be copied in the UI
# mod_map_utils_ui("map_utils_ui_1")
    
## To be copied in the server
# callModule(mod_map_utils_server, "map_utils_ui_1")
 
