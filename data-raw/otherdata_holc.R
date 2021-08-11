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


## holc  ---------------
# ftp://ftp.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/plan_historic_holc_appraisal/gpkg_plan_historic_holc_appraisal.zip
temp <- tempfile()
download.file("ftp://ftp.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/plan_historic_holc_appraisal/gpkg_plan_historic_holc_appraisal.zip",
  destfile = temp
)
redline <- sf::read_sf(unzip(temp, "plan_historic_holc_appraisal.gpkg")) %>%
  st_transform(4326) %>%
  filter(HSG_SCALE == "Hazardous" | HSG_SCALE == "Definitely Declining") %>%
  st_union()

fs::file_delete("plan_historic_holc_appraisal.gpkg")

# levels(as.factor(redline$HSG_SCALE))
# redline %>% ggplot()+geom_sf()
usethis::use_data(redline, overwrite = TRUE)
