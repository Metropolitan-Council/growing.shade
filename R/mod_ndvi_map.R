#' ndvi_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ndvi_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    # actionButton(ns("ndviButton"), "Detailed priority zones", class = "btn-success"),
    
    leafletOutput(ns("ndvimap"), height = 600)#, width = 300)#,
    
  )
}
    
#' ndvi_map Server Function
#'
#' @noRd 
mod_ndvi_map_server <- function(input, output, session,
                                tract_selections = tract_selections){
  ns <- session$ns
  
  tract_geo <- reactive({
    tract_geo <- eva_tract_geometry %>% 
      filter(GEOID == tract_selections$selected_tract)
    
    return(tract_geo)
  })
  
  
  make_ndvi_vals <-  reactive({
    
    # tract_geo <- eva_tract_geometry %>% 
    #   filter(GEOID == tract_selections$selected_tract)
    
    lowndvi_raster <- raster::raster("./data/lowndvi.tif") %>%
      # raster::crop(eva_tract_geometry %>%
      # filter(GEOID == "27053980000"))
      raster::crop(tract_geo())
    
    crs(lowndvi_raster) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    return(lowndvi_raster)
  })
  
  
  # lowndvi_raster <- raster::raster("./data/lowndvi.tif") %>%
  #   raster::crop(eva_tract_geometry %>%
  #   filter(GEOID == "27053980000"))
  # leaflet() %>%
  #   addRasterImage(lowndvi_raster,
  #                  # colors = pal,
  #                  opacity = .7,
  #                  group = "NDVI") 
  
  output$ndvimap <- renderLeaflet({ 
    if(identical(tract_selections$selected_tract, character(0))) {
      print("nodata for ndvimap")
      
      leaflet() %>% 
        addLabelOnlyMarkers(
          lng = -92.89,
          lat = 45.01,
          label = HTML("Click on an area<br>in the Step 2 map<br>for detailed information <br>of areas which may<br>be suitable for new <br>tree plantings."),
          labelOptions = labelOptions(noHide = T,
                                      style = list(
                                      "color" = "#0054A4",
                                      "font-size" = "12px"))
      # setView(
      # lat = st_coordinates(map_centroid)[2], #44.963,
      # lng = st_coordinates(map_centroid)[1], #-93.22,
      # zoom = 12
    )
      } else{
        library(raster)
        # print(make_ndvi_vals())
        # pal <- colorNumeric(c("#67000d", "#fcbba1"), values(make_ndvi_vals()),
        #                     na.color = "transparent")
        
        pal <- colorBin("Reds", values(make_ndvi_vals()), pretty = T, na.color = "transparent", reverse = TRUE)

        leaflet() %>%
          
          addMapPane(name = "Aerial Imagery", zIndex = 200) %>%
          addProviderTiles(
            provider = providers$Esri.WorldImagery,
            group = "Aerial Imagery"
          ) %>%
          addProviderTiles("CartoDB.PositronOnlyLabels",
                           options = leafletOptions(pane = "Aerial Imagery"),
                           group = "Aerial Imagery") %>%
          
          addMapPane(name = "Map", zIndex = 200) %>%
          addProviderTiles("Esri.WorldTopoMap", #CartoDB.PositronNoLabels",
                           group = "Map"
          ) %>%
          # addProviderTiles("CartoDB.PositronOnlyLabels",
          #                  options = leafletOptions(pane = "Carto Positron"),
          #                  group = "Carto Positron") %>%
          addPolygons(data = tract_geo(),
                      color = "#0054A4",
                      fill = FALSE) %>%
          leaflet.extras::addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE) %>%
          
            addMapPane("NDVI", zIndex = 431) %>%
          addRasterImage(make_ndvi_vals(),
                         colors = pal,
                         opacity = .7,
                         group = "NDVI") %>%
          addLegend(pal = pal,
                    position = "topright",
                    values = values(make_ndvi_vals()),
                    title = "Greenness (NDVI)") %>%
          
          addLayersControl(
            position = "bottomright",
            baseGroups = c(
              "Aerial Imagery",
              "Map"
            ),
            overlayGroups = c(
              "NDVI"
            ))
      
    } 
      })
  
  
  #   
  #   addLayersControl(
  #     position = "bottomright",
  #     baseGroups = c(
  #       "Carto Positron",
  #       "Aerial Imagery"
  #     ),
  #     overlayGroups = c(
  #       "NDVI"
  #     ))
  # })
  # }
 
}
    
## To be copied in the UI
# mod_ndvi_map_ui("ndvi_map_ui_1")
    
## To be copied in the server
# callModule(mod_ndvi_map_server, "ndvi_map_ui_1")
 
