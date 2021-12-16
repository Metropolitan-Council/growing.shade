# water bodies -----------------
library(sf)
library(tidyverse)

temp <- tempfile()
download.file("ftp://ftp.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/water_lakes_rivers/gpkg_water_lakes_rivers.zip", destfile = temp)

river_lake <- sf::read_sf(unzip(temp, "water_lakes_rivers.gpkg")) %>%
  # st_union() %>%
  st_buffer(0) %>%
  st_transform(4326)

fs::file_delete("water_lakes_rivers.gpkg")

usethis::use_data(river_lake, overwrite = TRUE)
# 7.4 mb
# 7.3 mb with union
