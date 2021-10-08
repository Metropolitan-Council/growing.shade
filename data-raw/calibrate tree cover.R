# calibrate tree coverage from sentinel with lidar

library(sf)
wholearea <- metc_region %>%
  summarise(st_union(.))

# make a equal area grid; there are 704 tracts, so I want to make at least 1000 grids I think?
g = st_make_grid(wholearea,
                 n = c(36, 36)) %>% 
  st_intersection(wholearea)
length(g) #1015 grids
sf::st_write(g, "/Users/escheh/Documents/GitHub/planting.shade/storymap-info/shapefiles/metc_grid.shp", append = FALSE)

ggplot() +
  geom_sf(data = wholearea) +
  geom_sf(data = g,
          fill = "transparent")

