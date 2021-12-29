# ` script to download user tracts from tigris. works for 2 state/county combinations
source("data-raw/packages_global.R")
source("data-raw/shared_drive.R")

blocks_ctus <- readxl::read_xlsx(paste0(shared_drive, "/Crosswalks/BlockstoCOCTUs.xlsx")) %>%
  filter(YEAR == 2017) %>%
  mutate(GEOID = substr(BLK10, start = 1, stop = 11)) %>%
  group_by(GEOID, CTU_NAME, CTU_CODE) %>%
  count() %>%
  group_by(GEOID) %>%
  mutate(ncity = row_number()) %>%
  dplyr::select(-n) %>%
  pivot_wider(names_from = ncity, values_from = CTU_NAME, -CTU_CODE)

mn_tracts <- tigris::tracts(
  state = "MN",
  county = c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")
) %>%
  # st_buffer(0) %>% # 27053025100, 27037060725 is having a self intersection error
  # st_make_valid() %>%
  sf::st_transform(4326) %>%
  mutate(GEO_NAME = GEOID)
usethis::use_data(mn_tracts, overwrite = TRUE)

metc_region <- mn_tracts %>%
  group_by(COUNTYFP) %>%
  summarise(geometry = sf::st_union(geometry))
usethis::use_data(metc_region, overwrite = TRUE)



######
# but really, we want to expand across bigger region
# focus on midwest
# focus on historically forested states OR states with tallgrass prarire (and savannah) - NOT mixed or shortgrass prairie (algorithm doesn't work that well for those areas, and it's problimatic when thinking about biodiversity)
######
# mnwi_tracts <- tigris::tracts(state = "MN") %>%
#   bind_rows(tigris::tracts(state = "WI")) %>%
#   sf::st_transform(4326)
# usethis::use_data(mnwi_tracts, overwrite = TRUE)
#
# sf::write_sf(tigris::tracts(state = "MN") %>%
#   bind_rows(tigris::tracts(state = "WI")), "./mnwitracts.shp")


# filter(st_is_valid(mn_tracts)),

# ### if tract is small, turn geometry into a ctu
# temp <- tempfile()
# download.file(
#   "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/bdry_metro_counties_and_ctus/shp_bdry_metro_counties_and_ctus.zip",
#   destfile = temp
# )
# temp2 <- tempfile()
# unzip(zipfile = temp, exdir = temp2)
# list.files(temp2)
# # fs::file_delete("plan_parks_regional.gpkg")
#
# mn_ctus <- sf::read_sf(paste0(temp2, pattern = "/CTUs.shp")) %>%
#   sf::st_transform(4326)
#
# tract_ctus <- mn_tracts %>%
#   filter(ALAND <= 809371) %>%
#   st_drop_geometry() %>%
#   left_join(blocks_ctus) %>%
#   dplyr::select(GEO_NAME, `1`) %>%
#   rename(CTU_NAME = `1`) %>%
#   left_join(mn_ctus) %>%
#   st_as_sf()
#
# crop_tract_ctus <- mn_tracts %>%
#   filter(ALAND > 809371) %>%
#   bind_rows(tract_ctus) %>%
#   st_as_sf() %>%
#   sf::st_transform(4326)
#
# usethis::use_data(crop_tract_ctus, overwrite = TRUE)
#
# crop_tract_ctus %>% filter(GEO_NAME == "27053105400") %>%
#   ggplot() + geom_sf()
# crop_tract_ctus %>% filter(GEO_NAME == "27037061003") %>%
#   ggplot() + geom_sf()


######### if tract geo is small, add 1 mi buffer
buffer_tract <- mn_tracts %>%
  filter(ALAND <= 3000000) %>%
  # smaller than 1 mile
  st_transform(3857) %>%
  st_buffer(dist = 1609.34 * 1) %>%
  # the 3857 projection uses meters as a distance, so 1.0 mi = buffer here for ~5mi
  sf::st_transform(4326) %>%
  st_buffer(0)

buffer_tract_mid <- mn_tracts %>%
  filter(ALAND > 3000000, ALAND <= 5000000) %>%
  # greater than 1 mile, smaller than 2 miles
  st_transform(3857) %>%
  st_buffer(dist = 1609.34 * .5) %>%
  sf::st_transform(4326) %>%
  st_buffer(0)

buffer_tract_max <- mn_tracts %>%
  filter(ALAND > 5000000) %>%
  # all other tracts
  st_transform(3857) %>%
  st_buffer(dist = 1609.34 * .1) %>%
  sf::st_transform(4326) %>%
  st_buffer(0)

crop_tract_ctus <- buffer_tract_max %>%
  bind_rows(buffer_tract) %>%
  bind_rows(buffer_tract_mid) %>%
  st_as_sf() %>%
  sf::st_transform(4326) %>%
  st_buffer(0)

# filter(crop_tract_ctus, st_is_valid(crop_tract_ctus)=="TRUE")

usethis::use_data(crop_tract_ctus, overwrite = TRUE)
