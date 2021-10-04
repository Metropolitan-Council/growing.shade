#neighborhoods
#st paul here: https://information.stpaul.gov/City-Administration/District-Council-Shapefile-Map/dq4n-yj8b
#minneap here: https://opendata.minneapolismn.gov/datasets/communities/explore?location=44.970861%2C-93.261718%2C12.85
#brooklyn park here: but no dl: https://gis.brooklynpark.org/neighborhoodinfo/

library(sf)
library(tidyverse)

#centroids
# step 2 = process things
## functions to make this easier-----
# swp_sum <- function(...) {
#   totals <- swp %>%
#     sf::st_drop_geometry() %>%
#     group_by(!!!quos(...)) %>% 
#     summarise(total_vol_half = sum(VOL_HALF_F, na.rm = T),
#               total_solar_pot = sum(SOLAR_POT, na.rm = T),
#               total_area = sum(AREA, na.rm = T)) 
#   bldg_type <- swp %>%
#     sf::st_drop_geometry() %>%
#     group_by(!!!quos(...), FEATURE_TY) %>% 
#     summarise(vol_half = sum(VOL_HALF_F, na.rm = T),
#               solar_pot = sum(SOLAR_POT, na.rm = T),
#               area = sum(AREA, na.rm = T)) %>%
#     pivot_wider(names_from = "FEATURE_TY", values_from = c(vol_half:area))
#   geo_sum <- full_join(totals, bldg_type)
#   return(geo_sum)
# }
# # swp_sum(WMO_NAME)

swp_centroid <- function(x, ...) {
  points <- minneap %>%
    mutate(zoom = case_when(Shape_Area < 1e6 ~ 15,
                            Shape_Area < 1e8 ~ 13,
                            Shape_Area < 1e9 ~ 12,
                            TRUE ~ 11)) %>%
    st_transform(26915) %>%
    st_centroid() %>%
    st_transform(4326) %>%
    select(!!!quos(...), geometry, zoom) %>%
    mutate(lat = unlist(map(.$geometry,1)),
           long = unlist(map(.$geometry,2))) %>%
    sf::st_drop_geometry()
  geos <- x %>%
    select(!!!quos(...), geometry) %>%
    st_transform(4326)
  combo <- full_join(geos, points) %>%
    arrange(!!!(quos(...))) 
  return(combo)
}


### nhoods

minneap <- read_sf("./data-raw/minneapolis communities/Minneapolis_Communities.shp") %>%
  rename(GEO_NAME = CommName) %>%
  mutate(Shape_Area = as.numeric(st_area(.))) %>%
  swp_centroid(., GEO_NAME) %>%
  # select(nhood) %>%
  mutate(city = "Minneapolis")

stpaul <- read_sf("./data-raw/stpaul communities/geo_export_0c076f52-d6ff-4546-b9fa-bd9980de6e8a.shp") %>%
  mutate(Shape_Area = as.numeric(st_area(.))) %>%
  rename(GEO_NAME = name2) %>%
    swp_centroid(., GEO_NAME) %>%
    # select(nhood) %>%
  mutate(city = "St. Paul")

nhood_list <- bind_rows(minneap, stpaul) %>%
  st_transform(4326)

usethis::use_data(nhood_list, overwrite = TRUE)



####
#city

# step 1 ctus 
temp <- tempfile()
temp2 <- tempfile()
download.file(
  "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/bdry_metro_counties_and_ctus/shp_bdry_metro_counties_and_ctus.zip",
  destfile = temp
)
unzip(zipfile = temp, exdir = temp2)
list.files(temp2)

ctu_geo <-
  sf::read_sf(paste0(temp2, pattern = "/CTUs.shp")) %>%
  select(CTU_NAME, Shape_Area)# 

files <- list.files(temp2, full.names = T)
# files
file.remove(files)

## ctus ----------
ctu_list <- swp_centroid(ctu_geo, CTU_NAME) %>%
  # full_join(swp_sum(CTU_NAME))  %>%
  # filter(!is.na(lat))  %>%
  rename(GEO_NAME = CTU_NAME)
usethis::use_data(ctu_list, overwrite = TRUE)
