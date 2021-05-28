
library(raster)

s2 <- raster("./data-raw/greenest_map_metc_2020.tif") %>%
  round(., 1) #%>% #round ndvi values
  # trim() #get rid of nas
s2[s2 > .5 ] = NA #large ndvi values aren't relevant for this project
writeRaster(s2, './data/lowndvi.tif', overwrite=TRUE)


s2_trim <- trim(s2)

#plot
pal <- colorNumeric(c("#67000d", "#fcbba1"), values(s2),
                    na.color = "transparent")

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
                   group = "Aerial Imagery") %>%
  
  # addMapPane("NDVI", zIndex = 431) %>%
  addRasterImage(s2, 
                 colors = pal, 
                 opacity = .7,
                 group = "NDVI") %>%
  addLegend(pal = pal, 
            values = values(s2),
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



# s2_polys <- rasterToPolygons(s2_trim, na.rm = T, 
#                              dissolve = T) %>% 
#   st_as_sf()

#######
# show full ndvi values
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
  raster::crop(tract) %>%
  round(., 2) #round ndvi values

# pal <- colorNumeric(c("#67000d", "#fcbba1"),#greens("#FFFFFF", "#009a00"), 
#                     values(test),
#                     na.color = "transparent")
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(test),
                    na.color = "transparent")

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
                 # colors = pal, 
                 opacity = .7,
                 group = "NDVI") %>%
  # addLegend(pal = pal, 
  #           values = values(test),
  #           title = "Greenness (NDVI)") %>%
  
  addLayersControl(
    position = "bottomright",
    baseGroups = c(
      "Carto Positron",
      "Aerial Imagery"
    ),
    overlayGroups = c(
      "NDVI"
    ))

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
