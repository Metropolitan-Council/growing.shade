# calibrate tree coverage from sentinel with lidar

source("data-raw/packages_global.R")
wholearea <- metc_region %>%
  summarise(st_union(.))

# make a equal area grid; there are 704 tracts, so I want to make at least 1000 grids I think?
g <- st_make_grid(wholearea,
  n = c(36, 36)
) %>%
  st_intersection(wholearea)

geometry <- st_sfc(lapply(1:length(g), function(x) st_geometrycollection()))
df <- st_sf(id = 1:length(g), geometry = g)


sf::st_write(df, "~/Documents/GitHub/planting.shade/storymap-info/shapefiles/metc_grid.shp", append = FALSE)

ggplot() +
  geom_sf(data = wholearea) +
  geom_sf(
    data = df,
    fill = "transparent"
  )
