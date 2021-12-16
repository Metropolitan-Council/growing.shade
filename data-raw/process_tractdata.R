## code to prepare `eva_data_main` dataset goes here

date <- format(Sys.time(), "%Y%m%d")
# pkgload::load_all()

########
# # load packages -----
########
# # this works when working inside the package
requireNamespace("readxl", quietly = TRUE)
requireNamespace("fs", quietly = TRUE)
requireNamespace("janitor", quietly = TRUE)
requireNamespace("tidyverse", quietly = TRUE)
library(tidyverse)
library(tigris)
library(sf)


####################
# tract geographies
#####################
#### tracts ---------
mn_tracts_1 <- tigris::tracts(
  state = "MN",
  county = c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")
) %>%
  sf::st_transform(4326) %>%
  mutate(GEO_NAME = GEOID)




################
# process google earth engine data
##################
# # GEE data is in repo "users/ehe/MetCoucil/GrowingShade_CanopyCoverage"
# # https://code.earthengine.google.com/a0da66053ecb26b668df4297c4ebed59
#
# # ndvi
# tract_ndvi <- read_csv("./data-raw/meanNDVI_tracts_year2020.csv",
#                        col_types = cols(GEOID10 = "c", `system:index` = "c", Year = 'd', ndvi = 'd', `.geo` = 'c')) %>%
#   select(-`system:index`, -.geo, -Year)
# # filter(tract_ndvi, ndvi == "No data")
#
# # canopy coverage
# canopy <- read_csv("./data-raw/TreeAcres_tracts_year2020.csv",
#                    col_types = cols(.default = "d", GEOID10 = "c")) %>%
#   left_join(sf::st_drop_geometry(mn_tracts_1), by = c("GEOID10" = "GEOID")) %>%
#   transmute(GEOID10 = GEOID10,
#             treeacres = `1`,
#             landacres = ALAND / 4046.86,
#             canopy_percent = treeacres / landacres / 2) #it has a % so use fraction instead; and halve itbecuase 10x10 is big
#
#
# # tree raster
# treecrs <- raster::raster("./data/TreeMap_crs4326_2020.tif")
# treecrs
#
# cuts=c(0, 1) #set breaks
# pal <- colorRampPalette(c("white","green"))
# plot((treecrs %>% crop(filter(ctu_list, GEO_NAME == "Afton"))), breaks=cuts, col = pal(7)) #plot with defined breaks
# plot((treecrs %>% crop(filter(ctu_list, GEO_NAME == "Afton")))) #plot with defined breaks


# # I did this to crop NDVI, but I'm not using NDVI anymore
# test <- reclassify(treecrs, cbind(-Inf, .5, NA), right=FALSE)
# raster::writeRaster(test, './data/tree_raster.tif', overwrite=TRUE)

# test <- raster(x = "./data/BinaryTreeMap_crs4326_2020.tif") %>% #this is from -999 to 1; not so good
#   crop(filter(ctu_list, GEO_NAME == "Lake Elmo"))
# plot(test)


##############
# ctu and nhood summaries + geographies
#############
temp <- tempfile()
download.file("ftp://ftp.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/water_lakes_rivers/gpkg_water_lakes_rivers.zip", destfile = temp)
river_lake_buffer <- sf::read_sf(unzip(temp, "water_lakes_rivers.gpkg")) %>%
  filter(NAME_DNR %in% c("Mississippi", "Minnesota")) %>% # these rivers are boundaries
  st_buffer(200) %>% # add 10m buffer
  # st_simplify(dTolerance = 100) %>%
  # st_buffer(300) %>%
  st_union() %>%
  st_buffer(0)


# fxns to make easy -----
st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))

# find centroid of geographies
find_centroid <- function(x, ...) {
  points <- x %>%
    mutate(zoom = case_when(
      Shape_Area < 1e6 ~ 15,
      Shape_Area < 1e8 ~ 13,
      Shape_Area < 1e9 ~ 12,
      TRUE ~ 11
    )) %>%
    st_transform(26915) %>%
    st_centroid() %>%
    st_transform(4326) %>%
    select(!!!quos(...), geometry, zoom) %>%
    mutate(
      lat = unlist(map(.$geometry, 1)),
      long = unlist(map(.$geometry, 2))
    ) %>%
    sf::st_drop_geometry()
  geos <- x %>%
    select(!!!quos(...), city, geometry) %>%
    st_transform(4326)
  combo <- full_join(geos, points) %>%
    arrange(!!!(quos(...)))
  return(combo)
}

tree_summary <- function(x) {
  x %>%
    st_transform(26915) %>%
    st_buffer(0) %>%
    st_buffer(-200) %>% # give it a bit of a buffer
    st_erase(river_lake_buffer) %>% # erase remaining rivers
    st_intersection(mn_tracts_1 %>%
      select(GEOID) %>%
      st_transform(26915)) %>%
    left_join(canopy %>%
      rename(GEOID = GEOID10),
    by = "GEOID"
    ) %>%
    st_drop_geometry() %>%
    group_by(GEO_NAME) %>%
    summarise(
      min = round(min(canopy_percent) * 100, 1),
      max = round(max(canopy_percent) * 100, 1),
      ntracts = n()
    )
}

### neighborhoods -----------
# st paul here: https://information.stpaul.gov/City-Administration/District-Council-Shapefile-Map/dq4n-yj8b
# minneap here: https://opendata.minneapolismn.gov/datasets/communities/explore?location=44.970861%2C-93.261718%2C12.85
# brooklyn park here: but no dl: https://gis.brooklynpark.org/neighborhoodinfo/

minneap <- read_sf("./data-raw/minneapolis communities/Minneapolis_Communities.shp") %>%
  rename(GEO_NAME = CommName) %>%
  mutate(Shape_Area = as.numeric(st_area(.))) %>%
  mutate(city = "Minneapolis")

stpaul <- read_sf("./data-raw/stpaul communities/geo_export_0c076f52-d6ff-4546-b9fa-bd9980de6e8a.shp") %>%
  mutate(Shape_Area = as.numeric(st_area(.))) %>%
  rename(GEO_NAME = name2) %>%
  mutate(city = "St. Paul")

nhood_list <- bind_rows(minneap, stpaul) %>%
  find_centroid(., GEO_NAME) %>%
  full_join(tree_summary(.)) %>%
  arrange(city, GEO_NAME) %>%
  st_transform(4326)

usethis::use_data(nhood_list, overwrite = TRUE)

#### ctus -----------
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
  select(CTU_NAME, Shape_Area) #

## ctus ----------
ctu_list <- sf::read_sf(paste0(temp2, pattern = "/CTUs.shp")) %>%
  select(CTU_NAME, Shape_Area) %>%
  add_column(city = "doesn't matter") %>%
  find_centroid(., CTU_NAME) %>%
  select(-city) %>%
  rename(GEO_NAME = CTU_NAME) %>%
  full_join(tree_summary(.)) %>%
  arrange(GEO_NAME)

files <- list.files(temp2, full.names = T)
file.remove(files)

usethis::use_data(ctu_list, overwrite = TRUE)


###########
# link tracts to ctus and nhoods
#########
ctu_crosswalk <- ctu_list %>%
  select(GEO_NAME) %>%
  st_transform(26915) %>%
  st_buffer(-200) %>% # go up to -80 because carver
  st_erase(river_lake_buffer) %>% # erase remaining rivers
  st_intersection(mn_tracts_1 %>%
    select(GEOID) %>%
    rename(tract_id = GEOID) %>%
    st_transform(26915)) %>%
  st_drop_geometry()


test <- "Fort Snelling (unorg.)"
mn_tracts_1 %>%
  right_join(ctu_crosswalk %>% filter(GEO_NAME == test),
    by = c("GEOID" = "tract_id")
  ) %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = filter(ctu_list, GEO_NAME == test), fill = NA, color = "blue")

# filter(wide_ctu_crosswalk_1, !is.na(`...7`)) %>% data.frame()
library(leaflet)
library(sf)
library(tidyverse)
leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = filter(ctu_list, GEO_NAME %in% c("South St. Paul")) %>%
      st_transform(26915) %>% st_erase(river_lake_buffer) %>% st_buffer(-200) %>% st_transform(4326),
    color = "purple"
  ) %>%
  addPolygons(
    data = filter(mn_tracts_1, GEO_NAME == "27123980000") # %>%
    # right_join(ctu_crosswalk %>% filter(GEO_NAME == test), by = c("GEOID" = "tract_id"))
  ) # %>%
# addPolygons(data = river_lake %>% st_transform(4326), color = "red")

filter(ctu_crosswalk, tract_id == "27123980000")
nhood_crosswalk <- nhood_list %>%
  select(GEO_NAME) %>%
  st_transform(26915) %>%
  st_buffer(-200) %>%
  st_erase(river_lake_buffer) %>% # erase remaining rivers
  st_intersection(mn_tracts_1 %>%
    select(GEOID) %>%
    rename(tract_id = GEOID) %>%
    st_transform(26915)) %>%
  st_drop_geometry()

# test <- "The Greater East Side"
# mn_tracts_1 %>%
#   right_join(nhood_crosswalk %>% filter(GEO_NAME == test),
#              by = c("GEOID" = "tract_id")) %>%
#   ggplot()+
#   geom_sf() +
#   geom_sf(data = filter(nhood_list, GEO_NAME ==test), fill = NA, color = "blue")

usethis::use_data(ctu_crosswalk, overwrite = TRUE)
usethis::use_data(nhood_crosswalk, overwrite = TRUE)

wide_ctu_crosswalk_1 <- ctu_crosswalk %>%
  group_by(tract_id) %>%
  count() %>%
  full_join(ctu_crosswalk) %>%
  add_column(cities = "cities") %>%
  pivot_wider(names_from = cities, values_from = GEO_NAME) %>%
  unnest_wider(cities)

wide_ctu_crosswalk <- wide_ctu_crosswalk_1 %>%
  mutate(jurisdiction = paste(`...1`, `...2`, `...3`, `...4`, `...5`, `...6`, `...7`, sep = ", ")) %>%
  select(tract_id, jurisdiction) %>%
  mutate(
    jurisdiction = str_replace(jurisdiction, ", NA", ""),
    jurisdiction = str_replace(jurisdiction, ", NA", ""),
    jurisdiction = str_replace(jurisdiction, ", NA", ""),
    jurisdiction = str_replace(jurisdiction, ", NA", ""),
    jurisdiction = str_replace(jurisdiction, ", NA", ""),
    jurisdiction = str_replace(jurisdiction, ", NA", ""),
    jurisdiction = str_replace(jurisdiction, ", NA", "")
  ) %>%
  rename(GEOID = tract_id)
usethis::use_data(wide_ctu_crosswalk, overwrite = TRUE)


mn_tracts <- mn_tracts_1 %>%
  full_join(wide_ctu_crosswalk)
usethis::use_data(mn_tracts, overwrite = TRUE)
###################
# download equity considerations dataset
###################
temp <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_equity_considerations/xlsx_society_equity_considerations.zip",
  destfile = temp
)

equity <- readxl::read_xlsx(unzip(temp, "EquityConsiderations_Full.xlsx")) %>%
  janitor::clean_names()

fs::file_delete("EquityConsiderations_Full.xlsx")


## --------------variables of interest from equity considerations
equity_data_raw <- equity %>%
  select(
    tr10,
    ppov185,
    prim_flood,
    pwhitenh,
    p_0017,
    p_65up,
    avg_temp,
    phhi_qntl1,
    green_roof,
    env_cancer,
    luse_green,
    tr_ej,
    holc_pred,
    mdhhincnow,
    pd_any,
    pblacknh,
    pasiannh,
    phisppop,
    pamindnh,
    pwk_nowork,
    pownhome
  ) %>%
  rowwise() %>%
  mutate(
    luse_notgreen = 1 - luse_green,
    pbipoc = 1 - pwhitenh,
    holc_pred = if_else(is.na(holc_pred), 0, holc_pred),
    sens_age = p_0017 + p_65up
  ) %>% # "mutate" reformats any variables that need it
  select(
    -luse_notgreen, # and then I want to remove the variable I don't need anymore
    -pwhitenh
  )


##########
# CDC health data
#########
# variable options are documented here: https://www.cdc.gov/places/measure-definitions/index.html
# api token: https://chronicdata.cdc.gov/profile/edit/developer_settings

library("RSocrata")
# metadata https://dev.socrata.com/foundry/chronicdata.cdc.gov/cwsq-ngmh

health <- read.socrata(
  "https://chronicdata.cdc.gov/resource/cwsq-ngmh.json?$where=countyfips in('27003', '27019', '27037', '27053', '27123', '27139', '27163')",
  app_token = "D1kEEJEDVBpDppDIdDmwNXeVT",
  email = "ellen.esch@metc.state.mn.us",
  password = "TQfY5%Q3xY"
) %>%
  rename(GEOID10 = locationname)

health
names(health)
levels(as.factor(health$measure))

health2 <- health %>%
  filter(measure %in% c(
    "Current asthma among adults aged >=18 years",
    "Chronic obstructive pulmonary disease among adults aged >=18 years",
    "Mental health not good for >=14 days among adults aged >=18 years",
    "Physical health not good for >=14 days among adults aged >=18 years"
  )) %>%
  select(GEOID10, measureid, data_value) %>%
  mutate(data_value = as.numeric(data_value) / 100) %>% # change to fraction
  pivot_wider(names_from = measureid, values_from = data_value) %>%
  rename(tr10 = GEOID10)

###################
# combine data sources
###################

eva_data_raw <- equity_data_raw %>%
  full_join(canopy %>% rename(tr10 = GEOID10)) %>%
  full_join(tract_ndvi %>% rename(tr10 = GEOID10)) %>%
  full_join(health2) %>%
  mutate(
    ndvi2 = ndvi,
    canopy_percent2 = canopy_percent
  ) %>%
  rename(tract_string = tr10) # and for this project, I need to rename the tract variable

###################
# add some human-readable metadata
###################

## -------------------------------describe data
# cc (climate change preset) = prim_flood, avg_temp, ndvi
# ej (environmental justice preset) = pbipoc, phhi_qntl1, prim_flood, avg_temp, ndvi
# ph (public health preset)
eva_data_codes <- tribble(
  ~variable, ~name, ~type, ~interpret_high_value, ~cc, ~ej, ~ph, ~cons,
  "ppov185", "% people with income <185% of the poverty threshold", "people", "high_opportunity", 0, 1, 0, 0,
  "prim_flood", "% developed acres in primary flood zone", "environment", "high_opportunity", 1, 1, 0, 0,
  "pbipoc", "% people of color", "people", "high_opportunity", 0, 1, 0, 0,
  "p_0017", "% people age 17 or younger", "people", "high_opportunity", 0, 0, 0, 0,
  "p_65up", "% people age 65 or older", "people", "high_opportunity", 0, 0, 0, 0,
  "avg_temp", "Land surface temp on hot summer day", "environment", "high_opportunity", 1, 1, 1, 0,
  # "phhi_qntl1", "% households with annual income less than $35,000 (bottom quintile of households)", "people",  "high_opportunity", 0, 1, 0, 0,
  # "green_roof", "Water holding potential of green roofs on commercial bldgs", "environment",  "high_opportunity",
  "env_cancer", "Lifetime cancer risk from air toxics", "people", "high_opportunity", 0, 1, 1, 0,
  # "luse_notgreen", "% of tract NOT used for green space", "environment", "high_opportunity"
  "ndvi", "Average greenness (2020 NDVI)", "tree", "low_opportunity", 1, 0, 1, 0,
  "ndvi2", "Average greenness (2020 NDVI) - for conservation", "tree", "high_opportunity", 0, 0, 0, 1,
  "tr_ej", "Area of Environmental Justice Concern", "people", "high_opportunity", 0, 1, 0, 0,
  "holc_pred", "Share of tract's land acreage redlined", "people", "high_opportunity", 0, 1, 0, 0,
  "canopy_percent", "% tree canopy coverage in 2020", "tree", "low_opportunity", 1, 0, 1, 0,
  "canopy_percent2", "% tree canopy coverage in 2020 - for conservation", "tree", "high_opportunity", 0, 0, 0, 1,
  "mdhhincnow", "Median household income, 2015-2019 period (in 2019 dollars)", "people", "low_opportunity", 0, 0, 0, 0,
  "sens_age", "% people 17 or younger and 65 or older", "people", "high_opportunity", 0, 0, 1, 0,
  "pd_any", "% people with any disability", "people", "high_opportunity", 0, 0, 0, 0,
  "pblacknh", "% residents who identify as Black or African American, non-Latino", "people", "high_opportunity", 0, 0, 0, 0,
  "pasiannh", "% residents who identify as Asian, non-Latino", "people", "high_opportunity", 0, 0, 0, 0,
  "phisppop", "% residents who identify as Hispanic or Latino", "people", "high_opportunity", 0, 0, 0, 0,
  "pamindnh", "% residents who identify as Indigenous, non-Latino", "people", "high_opportunity", 0, 0, 0, 0,
  "pwk_nowork", "% of residents age 16-64 who did not work in past 12 months", "people", "high_opportunity", 0, 0, 0, 0,
  "pownhome", "% of residents who own their home", "people", "high_opportunity", 0, 0, 0, 0,
  "MHLTH", "Mental health not good for >=14 days among adults aged >=18 years (%)", "people", "high_opportunity", 0, 0, 0, 0,
  "PHLTH", "Physical health not good for >=14 days among adults aged >=18 years (%)", "people", "high_opportunity", 0, 0, 0, 0,
  "COPD", "Chronic obstructive pulmonary disease among adults aged >=18 years (%)", "people", "high_opportunity", 0, 0, 0, 0,
  "CASTHMA", "Current asthma among adults aged >=18 years (%)", "people", "high_opportunity", 0, 0, 0, 0
)
eva_data_codes %>% filter(ph == 1)

###################
# #create final dataset - no spatial data here
# #note: spatial data should be joined after any summarizing is done to save some computation time
###################

# #long data
eva_data_main <- eva_data_raw %>%
  pivot_longer(names_to = "variable", values_to = "raw_value", -tract_string) %>% # end the code after this line if you just want the reshaped data
  group_by(variable) %>%
  mutate(
    MEAN = mean(raw_value, na.rm = T),
    SD = sd(raw_value, na.rm = T),
    MIN = min(raw_value, na.rm = T),
    MAX = max(raw_value, na.rm = T),
    COUNT = as.numeric(sum(!is.na(raw_value))),
    z_score = (raw_value - MEAN) / SD
  ) %>%
  right_join(eva_data_codes, by = "variable") %>%
  # #we want high opportunity to be a high value, so this reorders those values if needed
  # mutate(opportunity_zscore = case_when(interpret_high_value == "high_opportunity" ~ z_score,
  #                                       interpret_high_value == "low_opportunity" ~ z_score * (-1),
  #                                         TRUE ~ NA_real_)) %>%

  # create nominal weights
  mutate(weights_nominal = case_when(
    interpret_high_value == "high_opportunity" ~ (raw_value - MIN) / (MAX - MIN) * 10,
    interpret_high_value == "low_opportunity" ~ 10 - (raw_value - MIN) / (MAX - MIN) * 10,
    TRUE ~ NA_real_
  )) %>%
  # Weights Standard Score
  mutate(weights_scaled = case_when(
    interpret_high_value == "high_opportunity" ~ pnorm(z_score) * 10,
    interpret_high_value == "low_opportunity" ~ (10 - pnorm(z_score) * 10),
    TRUE ~ NA_real_
  )) %>%
  # weights rank
  mutate(weights_rank = case_when(
    interpret_high_value == "high_opportunity" ~ min_rank(desc(weights_nominal)) / COUNT * 10,
    interpret_high_value == "low_opportunity" ~ min_rank(desc(weights_nominal)) / COUNT * 10,
    TRUE ~ NA_real_
  )) %>%
  # #rank
  mutate(overall_rank = case_when(
    interpret_high_value == "high_opportunity" ~ min_rank(desc(as.numeric(weights_nominal))),
    interpret_high_value == "low_opportunity" ~ min_rank(desc(as.numeric(weights_nominal)))
  )) %>%
  #
  # clean
  select(-MEAN, -SD, -MIN, -MAX) %>%
  full_join(wide_ctu_crosswalk %>% rename(tract_string = GEOID))

########
# save data
########

# this works if you're in a package
usethis::use_data(eva_data_main, overwrite = TRUE)

# otherwise use this
# write_csv(eva_data_main, "./eva_data_main.csv")

# ########
# # create metadata
# #########
md1 <- eva_data_main %>%
  group_by(variable) %>%
  summarise(
    MEANRAW = mean(raw_value, na.rm = T),
    MEANSCALED = mean(weights_scaled, na.rm = T)
  )
metadata <- eva_data_main %>%
  dplyr::group_by(type, name, variable, interpret_high_value, cc, ej, ph, cons) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  full_join(md1)

usethis::use_data(metadata, overwrite = TRUE)

#
#
# #####
# # create list of cities
# ####
# ctus <- levels(as.factor(equity$ctu_prmry)) #%>% as_tibble()
# usethis::use_data(ctus, overwrite = TRUE)
#
