#neighborhoods
#st paul here: https://information.stpaul.gov/City-Administration/District-Council-Shapefile-Map/dq4n-yj8b
#minneap here: https://opendata.minneapolismn.gov/datasets/communities/explore?location=44.970861%2C-93.261718%2C12.85
#brooklyn park here: but no dl: https://gis.brooklynpark.org/neighborhoodinfo/

library(sf)
library(tidyverse)

minneap <- read_sf("./data-raw/minneapolis communities/Minneapolis_Communities.shp") %>%
  rename(nhood = CommName) %>%
  select(nhood) %>%
  mutate(city = "Minneapolis")

stpaul <- read_sf("./data-raw/stpaul communities/geo_export_0c076f52-d6ff-4546-b9fa-bd9980de6e8a.shp") %>%
  rename(nhood = name1) %>%
  select(nhood) %>%
  mutate(city = "St. Paul") %>%
  st_transform()

nhood <- bind_rows(minneap, stpaul) %>%
  st_transform(4326)

usethis::use_data(nhood, overwrite = TRUE)
