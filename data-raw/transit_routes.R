# prepare transit routes

requireNamespace("readxl", quietly = TRUE)
requireNamespace("fs", quietly = TRUE)
requireNamespace("tigris", quietly = TRUE)
requireNamespace("janitor", quietly = TRUE)

library(dplyr)
library(fs)
library(sf)
library(tigris)
library(janitor)


# ## transit routes ---------------
# temp <- tempfile()
# download.file("ftp://ftp.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_transit_routes/gpkg_trans_transit_routes.zip",
#   destfile = temp
# )
# trans_routes <- sf::read_sf(unzip(temp, "trans_transit_routes.gpkg")) %>%
#   st_transform(4326)
# 
# fs::file_delete("trans_transit_routes.gpkg")
# 
# usethis::use_data(trans_routes, overwrite = TRUE)
# 


### ------
pot <- readxl::read_xlsx("/Volumes/shared/CommDev/Research/Research/Tableau/Surface with Purpose/SolarPotential_PolygonTable for Cam.xlsx")

head(pot) %>% data.frame()


pot2 <- readxl::read_xlsx("/Volumes/shared/CommDev/Research/Research/Tableau/Surface with Purpose/SolarPotential_PolygonTable.xlsx")

head(pot2) %>% data.frame()


parcels <- st_read("/Volumes/shared/CommDev/Research/Research/Tableau/Surface with Purpose/Surface with Purpose CTUs.shp") #so this is actually just a shape file fo each ctu, not the parcels within the ctu

parcels %>% filter(CTU_NAME == "Afton") %>%
  ggplot() + geom_sf(fill = "transparent")

pot %>% filter(PARCID == "ANOK_100067")
## transit stops ---------------
temp <- tempfile()
temp2 <- tempfile()
download.file("ftp://ftp.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_transit_stops/shp_trans_transit_stops.zip",
  destfile = temp
)

unzip(zipfile = temp, exdir = temp2) # list.files(temp2)
trans_stops <- sf::read_sf(paste0(temp2, pattern = "/TransitStops.shp")) %>%
  filter(
    busstop_yn == "Y",
    board_flag == 1
  ) %>%
  st_transform(4326)

# trans_stops %>% ggplot() + geom_sf(aes(col = busstop_yn))

usethis::use_data(trans_stops, overwrite = TRUE)
