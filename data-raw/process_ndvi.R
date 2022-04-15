
source("data-raw/packages_global.R")

# # # whole region NDVI - lakes/rivers removed
# s2 <- raster("./data-raw/greenest_map_metc_2020.tif") %>%
#   round(., 2) #%>% #round ndvi values
#   # trim() #get rid of nas
# s2[s2 > .6 ] = NA #large ndvi values aren't relevant for this project
# writeRaster(s2, './data/lowndvi.tif', overwrite=TRUE)

# #NDVI by landuse
glu_100 <- raster("./data-raw/greenest2020_glu2016_treemask_ag.tif") %>%
  round(., 2)

glu_111_112_113_116 <- raster("./data-raw/greenest2020_glu2016_treemask_single.tif") %>%
  round(., 2)

glu_114_115 <- raster("./data-raw/greenest2020_glu2016_treemask_multi.tif") %>%
  round(., 2)

glu_120_130 <- raster("./data-raw/greenest2020_glu2016_treemask_retail_office.tif") %>%
  round(., 2)

glu_141_142_143 <- raster("./data-raw/greenest2020_glu2016_treemask_mixeduse.tif") %>%
  round(., 2)

glu_151 <- raster("./data-raw/greenest2020_glu2016_treemask_ind.tif") %>%
  round(., 2)

glu_160 <- raster("./data-raw/greenest2020_glu2016_treemask_inst.tif") %>%
  round(., 2)

glu_170_park <- raster("./data-raw/greenest2020_glu2016_treemask_park.tif") %>%
  round(., 2)

glu_173 <- raster("./data-raw/greenest2020_glu2016_treemask_golf.tif") %>%
  round(., 2)

glu_210 <- raster("./data-raw/greenest2020_glu2016_treemask_undev.tif") %>%
  round(., 2)

# trees
tree_raster <- raster("./data-raw/greenest2020_glu2016_trees.tif") # %>%
round(., 2)
tree_raster[tree_raster < 1] <- NA
writeRaster(tree_raster, "./data/tree_raster.tif", overwrite = TRUE)


tract <- tract_geometry %>%
  filter(GEOID10 == "27163070406") # c("27163070406", "27053126000"))# "27053109100")

pal <- colorNumeric(c("#67000d", "#fcbba1"), 0:1,
  na.color = "transparent"
)
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
    layerId = "labs"
  ) %>%
  addRasterImage(tree_raster %>% raster::crop(tract),
    color = "#ffffff",
    opacity = 0,
    layerId = "Trees",
    group = "Trees"
  ) %>%
  addRasterImage(glu_100 %>% raster::crop(tract),
    colors = pal,
    opacity = .7,
    layerId = "Agriculture",
    group = "Canopy gaps"
  ) %>%
  addRasterImage(glu_173 %>% raster::crop(tract),
    colors = pal,
    opacity = .7,
    layerId = "Golf courses",
    group = "Canopy gaps"
  ) %>%
  addRasterImage(glu_151 %>% raster::crop(tract),
    colors = pal,
    opacity = .7,
    layerId = "Industrial",
    group = "Canopy gaps"
  ) %>%
  addRasterImage(glu_160 %>% raster::crop(tract),
    colors = pal,
    opacity = .7,
    layerId = "Institutional",
    group = "Canopy gaps"
  ) %>%
  addRasterImage(glu_141_142_143 %>% raster::crop(tract),
    colors = pal,
    opacity = .7,
    layerId = "Mixed use",
    group = "Canopy gaps"
  ) %>%
  addRasterImage(glu_170_park %>% raster::crop(tract),
    colors = pal,
    opacity = .7,
    layerId = "Parks",
    group = "Canopy gaps"
  ) %>%
  addRasterImage(glu_114_115 %>% raster::crop(tract),
    colors = pal,
    opacity = .7,
    layerId = "Residential - multifamily",
    group = "Canopy gaps"
  ) %>%
  addRasterImage(glu_111_112_113_116 %>% raster::crop(tract),
    colors = pal,
    opacity = .7,
    layerId = "Residential - single family",
    group = "Canopy gaps"
  ) %>%
  addRasterImage(glu_120_130 %>% raster::crop(tract),
    colors = pal,
    opacity = .7,
    layerId = "Retail and office",
    group = "Canopy gaps"
  ) %>%
  addRasterImage(glu_210 %>% raster::crop(tract),
    colors = pal,
    opacity = .7,
    layerId = "Undeveloped",
    group = "Canopy gaps"
  ) %>%
  leaflet.multiopacity::addOpacityControls(
    layerId = c(
      "Agriculture",
      "Golf courses",
      "Industrial",
      "Institutional",
      "Mixed use",
      "Parks",
      "Residential - multifamily",
      "Residential - single family",
      "Retail and office",
      "Undeveloped",
      "Trees"
    ),
    collapsed = T, position = "bottomright",
    title = "<strong>Opacity control by land use</strong>",
    # renderOnLayerAdd = TRUE
  ) %>%
  addLegend(
    pal = pal,
    values = 0:1,
    title = "Greenness (NDVI)"
  ) %>%
  addLayersControl(
    position = "bottomright",
    baseGroups = c(
      "Aerial Imagery"
    ),
    overlayGroups = c(
      "Canopy gaps",
      "Trees"
    )
  )


# #######
# # map ndvi values
# #######
# tract_geometry <- tigris::tracts(
#   state = "MN",
#   county = c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington"),
#   class = "sf",
#   year = 2010
# )

###### raster to vector
# trees <- terra::rast("./data/tree_raster.tif") # %>%
tree_polys <- as.polygons(trees)
writeVector(tree_polys, "./data/tree_poly.shp", overwrite = TRUE)
sf::st_write(tree_polys, "./data/tree_poly.shp", append = FALSE)
# writeVector(tree_polys, "./data/tree_poly.shp")
# raster::rasterToPolygons(trees, "./data/tree_poly.shp", n= 16, dissolve = T, na.rm = T, fun=function(x){x>=1})
# raster::shapefile(trees, "./data/tree_poly.shp")
