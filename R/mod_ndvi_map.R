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
    )
      } else{
        library(raster)

        pal <- colorNumeric(c("#67000d", "#fcbba1"), 0:1,
                            na.color = "transparent")
        leaflet() %>%
          addMapPane(name = "Aerial Imagery", zIndex = 200) %>%
          addProviderTiles(
            provider = providers$Esri.WorldImagery,
            group = "Aerial Imagery",
            layerId = "base"
          ) %>%
          addProviderTiles("CartoDB.PositronOnlyLabels",
                           # options = leafletOptions(pane = "Aerial Imagery"),
                           group = "Aerial Imagery",
                           options = c(zIndex = 210),
                           layerId = "labs") %>%
            addPolygons(data = tract_geo(),
                        color = "#0054A4",
                        fill = FALSE,
                        layerId = "tractoutline") %>%
            leaflet.extras::addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE) %>%
          
          # addRasterImage(make_trees(),
          #                color = "#ffffff",
          #                opacity = 0,
          #                layerId = "Trees",
          #                group = "Trees") %>%
          
          # addRasterImage(ag %>%
          #                  raster::crop(tract_geo()),
          #                colors = pal,
          #                opacity = .7,
          #                layerId = "Agriculture",
          #                group = "Canopy gaps") %>%

          # addRasterImage(ind %>%
          #                  raster::crop(tract_geo()),
          #                colors = pal,
          #                opacity = .7,
          #                layerId = "Industrial",
          #                group = "Canopy gaps") %>%

          addRasterImage(inst %>%
                           raster::crop(tract_geo()),
                         colors = pal,
                         opacity = .7,
                         layerId = "Institutional",
                         group = "Canopy gaps") %>%

          # addRasterImage(mixed %>%
          #                  raster::crop(tract_geo()),
          #                colors = pal,
          #                opacity = .7,
          #                layerId = "Mixed use",
          #                group = "Canopy gaps") %>%
          
          addRasterImage(parkgc %>%
                           raster::crop(tract_geo()),
                         colors = pal,
                         opacity = .7,
                         layerId = "Parks and golf courses",
                         group = "Canopy gaps") %>%

          addRasterImage(residential %>%
                           raster::crop(tract_geo()),
                         colors = pal,
                         opacity = .7,
                         layerId = "Residential",
                         group = "Canopy gaps") %>%
          
          # addRasterImage(retoff %>%
          #                  raster::crop(tract_geo()),
          #                colors = pal,
          #                opacity = .7,
          #                layerId = "Retail and office",
          #                group = "Canopy gaps") %>%
          
          addRasterImage(undev %>%
                           raster::crop(tract_geo()),
                         colors = pal,
                         opacity = .7,
                         layerId = "Undeveloped",
                         group = "Canopy gaps") %>%
          
        
          leaflet.multiopacity::addOpacityControls(layerId = c(#"Agriculture",
                                                               # "Industrial",
                                                               "Institutional",
                                                               # "Mixed use",
                                                               "Parks and golf courses",
                                                               "Residential",
                                                               # "Retail and office",
                                                               "Undeveloped"
                                                               # "Trees"
                                                               ),
                                                   collapsed = T, position = "bottomright",
                                                   title = "<strong>Opacity control by land use</strong>",
                                                   # renderOnLayerAdd = TRUE
          ) %>%
          
          addLegend(pal = pal,
                    values = 0:1,
                    title = "Greenness (NDVI)") %>%
          addLayersControl(
            position = "bottomright",
            baseGroups = c(
              "Aerial Imagery"
            ),
            overlayGroups = c(
              "Canopy gaps"
              # "Trees"
            )) 
        
        
        
        
        
        
        
        # pal <- colorBin("Reds", values(make_ndvi_vals()), pretty = T, na.color = "transparent", reverse = TRUE)
        # 
        # leaflet() %>%
        #   
        #   addMapPane(name = "Aerial Imagery", zIndex = 200) %>%
        #   addProviderTiles(
        #     provider = providers$Esri.WorldImagery,
        #     group = "Aerial Imagery"
        #   ) %>%
        #   addProviderTiles("CartoDB.PositronOnlyLabels",
        #                    options = leafletOptions(pane = "Aerial Imagery"),
        #                    group = "Aerial Imagery") %>%
        #   
        #   addMapPane(name = "Map", zIndex = 200) %>%
        #   addProviderTiles("Esri.WorldTopoMap", 
        #                    group = "Map"
        #   ) %>%
        # 
        #   addPolygons(data = tract_geo(),
        #               color = "#0054A4",
        #               fill = FALSE) %>%
        #   leaflet.extras::addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE) %>%
        #   
        #     addMapPane("NDVI", zIndex = 431) %>%
        #   addRasterImage(make_ndvi_vals(),
        #                  colors = pal,
        #                  opacity = .7,
        #                  group = "NDVI") %>%
        #   addLegend(pal = pal,
        #             position = "topright",
        #             values = values(make_ndvi_vals()),
        #             title = "Greenness (NDVI)") %>%
        #   
        #   addLayersControl(
        #     position = "bottomright",
        #     baseGroups = c(
        #       "Aerial Imagery",
        #       "Map"
        #     ),
        #     overlayGroups = c(
        #       "NDVI"
        #     ))
      
    } 
      })
  

 
}
    
## To be copied in the UI
# mod_ndvi_map_ui("ndvi_map_ui_1")
    
## To be copied in the server
# callModule(mod_ndvi_map_server, "ndvi_map_ui_1")
 
