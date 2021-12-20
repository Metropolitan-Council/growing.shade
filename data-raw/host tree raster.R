#####
# host tree raster
#####
source("data-raw/packages_global.R")

# In python run this code:
# reference: https://gdal2tiles.readthedocs.io/en/latest/readme.html#basic-usage

# import gdal2tiles
#
# gdal2tiles.generate_tiles('~/Documents/GitHub/planting.shade/data/tree_raster.tif',
#                           '~/Documents/GitHub/treeraster/',
#                           zoom='8-17')


# issues; no color (would be good if tiles had that)

tiles <- "https://leonawicz.github.io/tiles/us48lr/tiles/{z}/{x}/{y}.png"
leaflet(
  options = leafletOptions(minZoom = 0, maxZoom = 15), width = "100%"
) %>%
  addProviderTiles("Stamen.Toner") %>%
  addTiles(tiles, options = tileOptions(opacity = 0.8))


tiles_tree <- "https://metropolitan-council.github.io/treeraster/tiles/{z}/{x}/{-y}.png"
leaflet(options = leafletOptions(minZoom = 7, maxZoom = 15)) %>%
  # options = leafletOptions(minZoom = 10, maxZoom = 16)) %>%
  # addProviderTiles("Stamen.Toner") %>% #provider = providers$Esri.WorldImagery) %>%
  addTiles() %>%
  # addTiles( "https://metropolitan-council.github.io/treeraster/tiles/{z}/{x}/{-y}.png",
  #           options = tileOptions(opacity =1)) %>%   #this is good for gdal2tiles
  # tiles/10/246/654.png #is an example of a gdaltile
  addTiles("https://metropolitan-council.github.io/treeraster/tiles/{z}/{x}/{y}",
    options = tileOptions(opacity = 1)
  ) %>% # this is good for google cloud

  setView(
    lat = 44.963,
    lng = -93.32,
    zoom = 9
  )
