
library(raster)
library(tidyverse)
library(leaflet)

# # whole region NDVI - lakes/rivers removed
s2 <- raster("./data-raw/greenest_map_metc_2020.tif") %>%
  round(., 2) #%>% #round ndvi values
  # trim() #get rid of nas
s2[s2 > .6 ] = NA #large ndvi values aren't relevant for this project
writeRaster(s2, './data/lowndvi.tif', overwrite=TRUE)

# #NDVI by landuse
glu_111_farm <- raster("./data-raw/greenest2020_glu2016_treemask_farm.tif") %>%
  round(., 2)
glu_113_sfd <- raster("./data-raw/greenest2020_glu2016_treemask_sfd.tif") %>%
  round(., 2)
glu_114_sfa<- raster("./data-raw/greenest2020_glu2016_treemask_sfa.tif") %>%
  round(., 2)
glu_115_mf <- raster("./data-raw/greenest2020_glu2016_treemask_mf.tif") %>%
  round(., 2)
glu_116_manuf <- raster("./data-raw/greenest2020_glu2016_treemask_manuf.tif") %>%
  round(., 2)
glu_170_park <- raster("./data-raw/greenest2020_glu2016_treemask_park.tif") %>%
  round(., 2)

tract<- tract_geometry %>%
  filter(GEOID10 == "27163070406")# c("27163070406", "27053126000"))# "27053109100")
testr <- glu_113_sfd %>%#glu_170_park %>%
  raster::crop(tract)
# testr[testr > .7 ] = NA
# #plot - uut plotting the whole council region is too huge

#try:
# over .7 during summer
# either under .5 before apr 15 OR over .2 in January --> grass will green before decidious trees, but only conif green in winter
#
# seasons <- raster("./data-raw/greenest2020_glu2016_treemask_sfd.tif") %>%
#   round(., 2)
# testr <- seasons %>%
#   raster::crop(tract_geometry %>% filter(GEOID10 == "27123030202"))
r <- raster(xmn = -2.8, xmx = -2.79,
            ymn = 54.04, ymx = 54.05,
            nrows = 30, ncols = 30)
values(r) <- matrix(1:900, nrow(r), ncol(r), byrow = TRUE)
crs(r) <- crs("+init=epsg:4326")

# Provide layerId, group or category to show opacity controls
# If not specified, will render controls for all layers


pal <- colorNumeric(c("#67000d", "#fcbba1"), 0:1,
                    na.color = "transparent")
leaflet() %>%
  # addMapPane(name = "Aerial Imagery", zIndex = 200) %>%
  addProviderTiles(
    provider = providers$Esri.WorldImagery,
    # group = "Aerial Imagery", 
    layerId = "base"
  ) %>%
  addProviderTiles("CartoDB.PositronOnlyLabels",
                   # options = leafletOptions(pane = "Aerial Imagery"),
                   # group = "Aerial Imagery"
                   options = c(zIndex = 210),
                   layerId = "labs") %>%
  # addMapPane(name = "sfd", zIndex = 210) %>%
  
  addRasterImage(glu_111_farm %>% raster::crop(tract),
                 colors = pal,
                 opacity = .7,
                 layerId = "Farmstead") %>%
  
  addRasterImage(glu_114_sfa %>% raster::crop(tract),
                 colors = pal,
                 opacity = .7,
                 layerId = "Single family attached residential") %>%
  
  addRasterImage(glu_113_sfd %>% raster::crop(tract),
                 colors = pal,
                 opacity = .7,
                 # group = "sfd",
                 layerId = "Single family detached residential") %>%
  
  addRasterImage(glu_116_manuf %>% raster::crop(tract),
                 colors = pal,
                 opacity = .7,
                 layerId = "Manufactured housing park") %>%
  
  addRasterImage(glu_115_mf %>% raster::crop(tract),
                 colors = pal,
                 opacity = .7,
                 layerId = "Multifamily residential") %>%
  
  # addMapPane(name = "Parks", zIndex = 220) %>%
  addRasterImage(glu_170_park %>% raster::crop(tract),
                 colors = pal,
                 opacity = .7,
                 # group = "parks",
                 layerId = "Parks") %>%

  
  
  leaflet.multiopacity::addOpacityControls(layerId = c("Farmstead",
                                                       "Single family attached residential",
                                                       "Single family detached residential", 
                                                       "Manufactured housing park",
                                                       "Multifamily residential",
                                                       "Parks"),
                                           collapsed = FALSE, position = "bottomright",
                                           title = "<strong>Opacity control by land use</strong>",
                                           # renderOnLayerAdd = TRUE
                                           ) %>%
  
  addLegend(pal = pal,
            values = 0:1,
            title = "Greenness (NDVI)") #%>%
  # addLayersControl(
  #   position = "bottomright",
  #   baseGroups = c(
  #     "Aerial Imagery"
  #   ),
  #   overlayGroups = c(
  #     "sfd",
  #     "parks"
  #   )) 


  leaflet.multiopacity::addOpacityControls(layerId = c("sfd", "parks"),
                                           collapsed = F, position = "topright", title = "Opacity control")
  # leaflet.opacity::addOpacitySlider(layerId = "sfd") %>%
  # leaflet.opacity::addOpacitySlider(layerId = "parks")



# s2_polys <- rasterToPolygons(s2_trim, na.rm = T, 
#                              dissolve = T) %>% 
#   st_as_sf()

#######
# map ndvi values
#######
tract_geometry <- tigris::tracts(
  state = "MN",
  county = c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington"),
  class = "sf",
  year = 2010
) 

tract<- tract_geometry %>%
  filter(GEOID10 == "27163070406")# "27053109100")

test <- s2 %>% 
  raster::crop(tract) #%>%
  # round(., 2) #round ndvi values

pal <- colorBin("Reds", values(test), pretty = T, na.color = "transparent", reverse = TRUE)

leaflet() %>%
  addMapPane(name = "Carto Positron", zIndex = 200) %>%
  addProviderTiles("CartoDB.PositronNoLabels",
                   group = "Carto Positron"
  ) %>%
  addProviderTiles("CartoDB.PositronOnlyLabels",
                   options = leafletOptions(pane = "Carto Positron"),
                   group = "Carto Positron") %>%
  
  addMapPane(name = "Aerial Imagery", zIndex = 200) %>%
  addProviderTiles(
    provider = providers$Esri.WorldImagery,
    group = "Aerial Imagery"
  ) %>%
  addProviderTiles("CartoDB.PositronOnlyLabels", 
                   options = leafletOptions(pane = "Aerial Imagery"),
                   group = "Aerial Imagery")%>%
  
  # addMapPane("NDVI", zIndex = 431) %>%
  addRasterImage(test, 
                 colors = pal,
                 opacity = .7,
                 group = "NDVI") %>%
  addLegend(pal = pal,
            values = values(test),
            title = "Greenness (NDVI)") %>%
  
  addLayersControl(
    position = "bottomright",
    baseGroups = c(
      "Carto Positron",
      "Aerial Imagery"
    ),
    overlayGroups = c(
      "NDVI"
    ))

#### 
# try bringing in landuse and mapping
####
glu <- 

# ######
# ### show only low ndvi as polygons
# #######
# polys <- rasterToPolygons(test, na.rm = T, dissolve = TRUE) %>% st_as_sf()
# 
# # polys %>% filter(layer < .5) %>%
# # ggplot()+
# #   geom_sf(aes(fill= layer))
# 
# polys2 <- polys %>% filter(layer <.5)
# 
# pal2 <- colorNumeric(c("#67000d", "#fcbba1"), (polys2$layer))#,
#                     # na.color = "transparent")
# 
# leaflet() %>%
#   addMapPane(name = "Carto Positron", zIndex = 200) %>%
#   addProviderTiles("CartoDB.PositronNoLabels",
#                    group = "Carto Positron"
#   ) %>%
#   addProviderTiles("CartoDB.PositronOnlyLabels",
#                    options = leafletOptions(pane = "Carto Positron"),
#                    group = "Carto Positron") %>%
#   
#   addMapPane(name = "Aerial Imagery", zIndex = 200) %>%
#   addProviderTiles(
#     provider = providers$Esri.WorldImagery,
#     group = "Aerial Imagery"
#   ) %>%
#   addProviderTiles("CartoDB.PositronOnlyLabels", 
#                    options = leafletOptions(pane = "Aerial Imagery"),
#                    group = "Aerial Imagery")%>%
#   
#   addPolygons(data = polys2 %>% st_transform(4326),
#               stroke = T,
#               color = councilR::colors$mtsRed,
#               weight = .25,
#               fillOpacity = .7,
#               group = "NDVI",
#               fillColor = ~pal2(polys2$layer),
#               highlightOptions = highlightOptions(
#                 stroke = TRUE,
#                 color = "white",
#                 weight = 6,
#                 bringToFront = TRUE,
#                 opacity = 1
#               ),
#               popup = ~paste0("NDVI: ", polys2$layer))  %>%
#   addLegend(pal = pal2, 
#             values = polys2$layer,
#             title = "Greenness (NDVI)",
#             opacity = 1) %>%
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
#   
