source("data-raw/packages_global.R")

###
# metc region
####
county_geos <- tigris::counties(state = "MN")
metc_geos <- county_geos %>%
  filter(NAME %in% c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")) %>%
  mutate(GEOG_LEVEL = "RGN") %>%
  group_by(GEOG_LEVEL) %>%
  summarise(geometry = st_union(geometry)) %>%
  mutate(area = as.numeric(st_area(.))) %>%
  st_transform(3857)

### land cover -------
temp <- tempfile()
download.file(
  "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dnr/biota_marschner_presettle_veg/gpkg_biota_marschner_presettle_veg.zip",
  destfile = temp
)

historic_veg <-
  sf::read_sf(unzip(temp, "biota_marschner_presettle_veg.gpkg")) %>%
  st_transform(3857) %>%
  st_intersection(metc_geos) %>%
  mutate(
    XCLASS = case_when(
      XCLASS == "Big Woods - Hardwoods (oak, maple, basswood, hickory)" ~ "Big Woods - Hardwoods",
      XCLASS == "Mixed Hardwood and Pine (Maple, White Pine, Basswood, etc)" ~ "Mixed Hardwood and Pine",
      TRUE ~ XCLASS
    )
  )
historic_veg %>% ggplot() +
  geom_sf() +
  geom_sf(data = metc_geos, fill = "transparent")

fs::file_delete("biota_marschner_presettle_veg.gpkg")
# save(historic_veg, file = "historic_veg.rda")

### bearing trees -------
# use cleaners from this paper: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0151935
# download S1 File: S1 File. Zip file for all data processing. https://doi.org/10.1371/journal.pone.0151935.s001
# Supplement_1/data/input/fullpaleon_conversion_v0.4.csv

spp_cleaner <- read_csv("./data-raw/fullpaleon_conversion_v0.4.csv",
  show_col_types = FALSE
) %>%
  filter(Domain %in% "Upper Midwest") %>%
  mutate(
    spp_code = tolower(`Level 1`),
    spp_name = tolower(`Level 3a`)
  ) %>%
  dplyr::select(spp_code, spp_name)

temp <- tempfile()
download.file(
  "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dnr/biota_original_pls_bearing_trees/gpkg_biota_original_pls_bearing_trees.zip",
  destfile = temp
)

bearing_trees <-
  sf::read_sf(unzip(temp, "biota_original_pls_bearing_trees.gpkg")) %>%
  st_transform(3857) %>%
  st_intersection(metc_geos) %>%
  mutate(spp_code = tolower(SPECIES)) %>%
  left_join(spp_cleaner)

levels(as.factor(bearing_trees$spp_name))

# bearing %>% ggplot()+geom_sf()# + geom_sf(data=metc)
fs::file_delete("biota_original_pls_bearing_trees.gpkg")
# save(bearing_trees, file = "bearing_trees.rda")

presettle_comp <- bearing_trees %>%
  st_drop_geometry() %>%
  # group_by(spp_name, VEGTYPE) %>%
  count(spp_name) %>%
  mutate(percent = n / sum(n) * 100) %>%
  arrange(-percent) %>%
  mutate(timepoint = 1895)



########
# msp trees
# MCPHERSON et al 2005 - https://www.itreetools.org/documents/329/Minneapolis%20Municipal%20Tree%20Resource%20Analysis.pdf
# THIS IS STREET trees
#######
msp_spp <- tribble(
  ~spp_name, ~percent,
  "ash", (14.4 + 1.7),
  "maple", (13.1 + 2.4 + 1.2 + 11.8),
  "elm", (9.9 + 2.3),
  "basswood", (7 + 1.6),
  "hackberry", 4.5,
  "other hardwood", (2.6 + 3.5 + 3.4 + .2 + .1 + .1),
  # "other", (2.6 + 3.5 + 3.4 + .2 + .1 +.1),
  "linden", 10.4,
  "honeylocust", 7.2,
  "ginko", 2.5
) %>%
  mutate(timepoint = 2005)



####
# msp 2
# NOWAK et al 2006 - https://www.nrs.fs.fed.us/pubs/rb/ne_rb166.pdf
# doesn't sum to 100, so don't look at this study
#######
msp_spp2 <- tribble(
  ~spp_name, ~percent,
  "ash", (21.6),
  "maple", (3.3 + 4.2 + 9.1),
  "elm", (17.1),
  "hackberry", 4.3,
  "mulberry", 4.3,
  "cedar", 4.8,
  "oak", 1.9
) %>%
  mutate(timepoint = 2006)


###
## msp usfs
# davey 2004 - https://www.nrs.fs.fed.us/data/urban/state/city/?city=6#ufore_data (table 1)
#  this is grey literature, so hiding this
#####
msp_spp3 <- read_csv("./data-raw/CITYSUM.csv") %>%
  rename(percent = `trees percent`) %>%
  group_by(spp_name) %>%
  summarise(percent = sum(percent)) %>%
  mutate(timepoint = 2004)

### combo
treebiodiv <- presettle_comp %>%
  # bind_rows(msp_spp) %>%
  # bind_rows(msp_spp2) %>%
  bind_rows(msp_spp3) %>%
  mutate(spp_name = case_when(
    spp_name == "cherry" ~ "other",
    spp_name == "ginko" ~ "other", # old survey
    spp_name == "willow" ~ "other",
    spp_name == "walnut" ~ "other",
    spp_name == "birch" ~ "other",
    spp_name == "pine" ~ "other",
    spp_name == "spruce" ~ "other",
    spp_name == "other hardwood" ~ "other",
    spp_name == "hickory" ~ "other",
    spp_name == "linden" ~ "other",
    spp_name == "honeylocust" ~ "other",
    spp_name == "crabapple" ~ "other",
    TRUE ~ spp_name
  )) %>%
  group_by(spp_name, timepoint) %>%
  summarise(percent = sum(percent))


usethis::use_data(treebiodiv, overwrite = TRUE)




treebiodiv %>%
  # mutate(spp_name2 = fct_reorder(spp_name, percent)) %>%
  ggplot(aes(x = fct_reorder(spp_name, percent, .desc = F), y = percent, col = as.factor(timepoint))) +
  geom_point() +
  theme_minimal() +
  coord_flip()

treebiodiv %>%
  # mutate(spp_name = fct_reorder(spp_name, (percent))) %>%
  ggplot(aes(x = as.factor(timepoint), y = percent, fill = spp_name)) + # fill = fct_reorder(spp_name, percent, .desc = F))) +
  geom_bar(stat = "identity", col = "black") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired")

treebiodiv %>%
  ggplot(aes(x = (timepoint), y = percent, fill = spp_name, shape = spp_name)) +
  geom_line(position = position_dodge(width = 10)) + # , aes(col = spp_name)) +
  geom_point( # aes(color = if_else(spp_name %in% c("oak", "ash", "elm"), "red", "")), #col = "black",
    size = 3, position = position_dodge(width = 10)
  ) +
  scale_fill_brewer(palette = "Paired", name = "Species") +
  scale_color_brewer(palette = "Paired", name = "Species") +
  scale_shape_manual(values = rep(c(21:25), 3), name = "Species") +
  councilR::council_theme() +
  labs(x = "Date", y = "Species composition (%)")
# facet_wrap(~spp_name)

# presettle_comp %>%
#   ggplot(aes(x = 1, y = n, fill = spp_name)) +
#   geom_bar(position = "fill", stat = "identity")

## carbon - https://www.fs.fed.us/nrs/pubs/jrnl/2013/nrs_2013_nowak_001.pdf
#  carbon - https://www.nrs.fs.fed.us/pubs/jrnl/2002/ne_2002_nowak_002.pdf
#  https://extension.umn.edu/courses-and-events/carbon-capture-challenge
# https://www.fs.fed.us/psw/publications/mcpherson/psw_2011_mcpherson009.pdf
