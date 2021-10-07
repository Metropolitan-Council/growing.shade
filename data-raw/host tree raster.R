#####
# host tree raster
#####

# In python run this code:
# reference: https://gdal2tiles.readthedocs.io/en/latest/readme.html#basic-usage

# import gdal2tiles
# 
# gdal2tiles.generate_tiles('/Users/escheh/Documents/GitHub/planting.shade/data/tree_raster.tif', 
#                           '/Users/escheh/Documents/GitHub/treeraster/', 
#                           zoom='8-17')


# issues; no color (would be good if tiles had that)

library(leaflet)
tiles <- "https://leonawicz.github.io/tiles/us48lr/tiles/{z}/{x}/{y}.png"
leaflet(
  options = leafletOptions(minZoom = 0, maxZoom = 16), width = "100%") %>%
  addProviderTiles("Stamen.Toner") %>%
  addTiles(tiles, options = tileOptions(opacity = 0.8)) %>% setView(lat = 44.963,
                                                                    lng = -93.32,
                                                                    zoom = 15
  )


library(leaflet)
tiles <- "https://github.com/Metropolitan-Council/treeraster/tree/main/tiles/{z}/{x}/{y}.png"
leaflet(
  options = leafletOptions(minZoom = 10, maxZoom = 16), width = "100%") %>%
  addProviderTiles(provider = providers$Esri.WorldImagery) %>%
  # addTiles() %>%
  addTiles(tiles, options = tileOptions(opacity =.5)) %>% 
  setView(
    lat = 44.963,
    lng = -93.32,
    zoom = 15
  )



####
# api key??
# AIzaSyAc-W63-L-vERFxXMFarhFa7Y_ktOWhsQc
