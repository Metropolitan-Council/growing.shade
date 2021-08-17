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
          label = HTML("<div style='font-size:12pt'>Click on an area<br>in the Priority Map<br>for detailed information <br>of areas which may<br>be suitable for new <br>tree plantings.</div>"),
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
 

# library(leaflet); library(tidyverse); library(raster); library(sf)
# pal <- colorNumeric(c("#67000d", "#fcbba1"), 0:1,
#                     na.color = "transparent")
# residential <- raster::raster("./data/greenest2020_glu2016_treemask_residential.tif")
# trees <- raster::raster("./data/tree_raster.tif")
# 
# test <- filter(eva_tract_geometry, GEOID == "27123031701")
# bg <- eva_tract_geometry %>% left_join(eva_data_main %>% filter(variable == "p_65up") %>% rename(GEOID = tract_string)) %>% st_transform(4326)
# leaflet()%>%
#   setView(lat = 44.97, lng = -93.05, zoom = 13
#   ) %>%
#   addMapPane(name = "Aerial Imagery", zIndex = 0) %>%
#   addMapPane("Priority scores", zIndex = 150) %>%
#   addProviderTiles(
#     provider = providers$Esri.WorldImagery,
#     group = "Aerial Imagery",
#     layerId = "base",
#     options = pathOptions(pane = "Aerial Imagery")
#   ) %>%
#   addProviderTiles("CartoDB.PositronOnlyLabels",
#                    group = "Aerial Imagery",
#                    options = c(zIndex = 210),
#                    layerId = "labs") %>%
#   addPolygons(
#     data = bg, #%>% filter(GEOID != "27123031701"),
#     group = "Priority scores",
#     stroke = TRUE,
#     color =  councilR::colors$suppGray,
#     opacity = 0.9,
#     weight = 0.5, #0.25,
#     fillOpacity = 0.7,
#     options = pathOptions(pane = "Priority scores")
#     ) %>%
#   addPolygons(data = test,
#               color = "#0054A4",
#               fill = FALSE,
#               layerId = "tractoutline") %>%
#   addRasterImage(trees %>%
#                  raster::crop(test),
#                colors = "black", #pal,
#                opacity = .7,
#                layerId = "residential",
#                group = "Trees") %>%
#   addLayersControl(
#         position = "bottomright",
#         baseGroups = c(
#           "Aerial Imagery"
#         ),
#         overlayGroups = c(
#           "Trees", "Priority scores"
#         ))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# leaflet()%>%
#   setView(lat = 44.97, lng = -93.05, zoom = 13
#   ) %>%
#   addMapPane("background_map", zIndex = 100) %>%  # Level 1: bottom
#   addMapPane("polygons", zIndex = 150) %>%        # Level 2: middle
#   addMapPane("labels", zIndex = 430) %>%          # Level 3: top
#   addProviderTiles(
#     providers$Esri.WorldImagery,
#     options = pathOptions(pane = "background_map"),
#     group = "aerial image"
#   ) %>%
#   addLayersControl(
#     position = "bottomright",
#     baseGroups = c("aerial image"),
#     overlayGroups = c("labels", "polygons")
#   )%>%
#   addPolygons(
#     data = bg, stroke = T, smoothFactor = 0.2,
#     fillOpacity = 0.5, #fillColor = ~color_pal(value),
#     options = pathOptions(pane = "polygons"),
#     group = "polygons"
#   )  %>%
#   addRasterImage(trees %>%
#                    raster::crop(test),
#                  colors = "black", #pal,
#                  opacity = .7,
#                  layerId = "residential",
#                  group = "labels")
#   # addMapPane(name = "Aerial Imagery", zIndex = -10) %>%
#   addMapPane("Priority scores", zIndex = 200) %>%
#   addMapPane("Trees", zIndex = 430) %>%
#   addProviderTiles(
#     provider = providers$Esri.WorldImagery,
#     group = "Aerial Imagery",
#     layerId = "base"
#   ) %>%
#   addProviderTiles("CartoDB.PositronOnlyLabels",
#                    group = "Aerial Imagery",
#                    options = c(zIndex = 210),
#                    layerId = "labs") %>%
#   addPolygons(
#     data = bg, #%>% filter(GEOID != "27123031701"),
#     group = "Priority scores",
#     stroke = TRUE,
#     color =  councilR::colors$suppGray,
#     opacity = 0.9,
#     weight = 0.5, #0.25,
#     fillOpacity = 0.7,
#     options = pathOptions(pane = "Priority scores")
#   ) %>%
#   addPolygons(data = test,
#               color = "#0054A4",
#               fill = FALSE,
#               layerId = "tractoutline") %>%
#   addRasterImage(trees %>%
#                    raster::crop(test),
#                  colors = "black", #pal,
#                  opacity = .7,
#                  layerId = "residential",
#                  group = "Trees") %>%
#   addLayersControl(
#     position = "bottomright",
#     baseGroups = c(
#       "Aerial Imagery"
#     ),
#     overlayGroups = c(
#       "Trees", "Priority scores"
#     ))
# 
# # nhd_wms_url <- "https://basemap.nationalmap.gov/arcgis/services/USGSTopo/MapServer/WmsServer"
# # leaflet() %>%
# #   setView(        lat = 44.963,
# #                   lng = -93.42,
# #                   zoom = 10) %>%
# #   addTiles('https://arcgis.metc.state.mn.us/server/rest/services/GISLibrary/GeneralizedLandUse2020/FeatureServer',
# #            options = providerTileOptions(noWrap = TRUE), group="World Imagery")
# #   # addWMSTiles(nhd_wms_url, layers = "0")
