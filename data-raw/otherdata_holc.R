# prepare transit routes

source("packages_global.R")


## holc  ---------------
# ftp://ftp.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/plan_historic_holc_appraisal/gpkg_plan_historic_holc_appraisal.zip
temp <- tempfile()
download.file("ftp://ftp.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/plan_historic_holc_appraisal/gpkg_plan_historic_holc_appraisal.zip",
  destfile = temp
)
redline <- sf::read_sf(unzip(temp, "plan_historic_holc_appraisal.gpkg")) %>%
  filter(HSG_SCALE == "Hazardous") %>% # | HSG_SCALE == "Definitely Declining") %>%
  st_union() %>%
  st_transform(4326)

fs::file_delete("plan_historic_holc_appraisal.gpkg")

# levels(as.factor(redline$HSG_SCALE))
# redline %>% ggplot()+geom_sf()
usethis::use_data(redline, overwrite = TRUE)
sf::st_write(redline, "/Volumes/shared/CommDev/Research/Research/EllenEsch/redline/redline.shp", append = FALSE)


## transit routes ---------------
temp <- tempfile()
download.file("ftp://ftp.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_transit_routes/gpkg_trans_transit_routes.zip",
  destfile = temp
)
trans_routes <- sf::read_sf(unzip(temp, "trans_transit_routes.gpkg")) %>%
  st_transform(4326)

fs::file_delete("trans_transit_routes.gpkg")

usethis::use_data(trans_routes, overwrite = TRUE)


# ctus -------
temp <- tempfile()
temp2 <- tempfile()
download.file(
  "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/bdry_metro_counties_and_ctus/shp_bdry_metro_counties_and_ctus.zip",
  destfile = temp
)
unzip(zipfile = temp, exdir = temp2)
list.files(temp2)
ctuoutline <-
  sf::read_sf(paste0(temp2, pattern = "/CTUs.shp")) %>%
  select(CTU_NAME) %>%
  st_transform(4326)

files <- list.files(temp2, full.names = T)
files
file.remove(files)

usethis::use_data(ctuoutline, overwrite = TRUE)
# make sure to restart R after processing ctus...it's causing it to fail for some reason.
