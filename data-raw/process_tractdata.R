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
# # # and this works when not inside a package
# library("readxl") #if you get an error message, run `install.packages('readxl')` or the equivalent
# library("janitor")
# library("tidyverse")


################
# process google earth engine data
##################
# GEE data is in repo "users/ehe/MetCoucil/GrowingShade_CanopyCoverage"
# https://code.earthengine.google.com/a0da66053ecb26b668df4297c4ebed59

# ndvi
tract_ndvi <- read_csv("./data-raw/meanNDVI_tracts_year2020.csv",
                       col_types = cols(GEOID10 = "c", `system:index` = "c", Year = 'd', ndvi = 'd', `.geo` = 'c')) %>%
  select(-`system:index`, -.geo, -Year)
# filter(tract_ndvi, ndvi == "No data")





# canopy coverage
canopy <- read_csv("./data-raw/TreeAcres_tracts_year2020.csv",
                   col_types = cols(.default = "d", GEOID10 = "c")) %>%
  left_join(sf::st_drop_geometry(mn_tracts), by = c("GEOID10" = "GEOID")) %>%
  transmute(GEOID10 = GEOID10, 
            treeacres = `1`,
            landacres = ALAND / 4046.86,
            canopy_percent = treeacres / landacres / 2) #it has a % so use fraction instead; and halve itbecuase 10x10 is big


# tree raster
treecrs <- raster::raster("./data/TreeMap_crs4326_2020.tif")
test <- reclassify(treecrs, cbind(-Inf, 0.5, NA), right=FALSE)
raster::writeRaster(test, './data/tree_raster.tif', overwrite=TRUE)

#this takes FOREEVER. not goood
# test <- rasterToPolygons(treecrs, dissolve = T)

#this ballons the raster file size. not good
# treecrs[treecrs == 0] <- NA
# raster::writeRaster(treecrs, './data/tree_raster.tif', overwrite=TRUE)


# library(tidyverse); library(leaflet); library(raster); library(leafem)
# tract<- mn_tracts %>%
#   filter(GEOID == "27163070406")# c("27163070406", "27053126000"))# "27053109100")

# leaflet() %>%
#   addMapPane(name = "Aerial Imagery", zIndex = 200) %>%
#   addProviderTiles(
#     provider = providers$Esri.WorldImagery,
#     group = "Aerial Imagery",
#     layerId = "base"
#   ) %>%
#   addProviderTiles("CartoDB.PositronOnlyLabels",
#                    # options = leafletOptions(pane = "Aerial Imagery"),
#                    group = "Aerial Imagery",
#                    options = providerTileOptions(maxZoom = 18),
#                    layerId = "labs") %>%
#   addPolygons(data = tract, fill = NA)%>%
# 
#   addRasterImage(treecrs %>% raster::crop(tract) ,
#                  # colors = pal,
#                  color = "green",
#                  opacity = 0.5,
#                  layerId = "Trees",
#                  group = "Trees")
# 
# plot(treecrs)# %>% raster::crop(tract), main="trees")
# # treecrs %>% raster::crop(tract) 

# 
# 
# library(leaflet)
# library(leafem)
# library(stars)
# 
# ## add 2 layers to 2 custom panes - doesn't work, both rendered on pane from last call
# leaflet() %>%
#   addTiles() %>%
#   addMapPane("left", 200) %>%
#   # addMapPane("right", 201) %>%
#   addProviderTiles(
#     providers$Esri.WorldImagery
#     , group = "carto_left"
#     , options = tileOptions(pane = "left")
#     , layerId = "leftid"
#   ) %>%
#   # addProviderTiles(
#   #   providers$Esri.WorldImagery
#   #   , group = "carto_right"
#   #   , options = tileOptions(pane = "right")
#   #   , layerId = "rightid"
#   # ) %>%
#   leafem:::addGeotiff(
#     file = "./data/TreeMap_crs4326_2020.tif"
#     , group = "april"
#     , layerId = "april_id"
#     , resolution = 96
#     , opacity = .7
#     , options = tileOptions(
#       pane = "left"
#     )
#     , colorOptions = leafem:::colorOptions("black")
#   ) #%>%
#   leafem:::addGeotiff(
#     file = tiffl_05
#     , group = "may"
#     , layerId = "may_id"
#     , resolution = 96
#     , opacity = 1
#     , options = tileOptions(
#       pane = "right"
#     )
#     , colorOptions = leafem:::colorOptions(
#       palette = pal
#       , breaks = brks
#       , na.color = "transparent"
#     )
#     , pixelValuesToColorFn = myCustomJSFunc
#   ) %>%
#   leaflet.extras2::addSidebyside(
#     layerId = "sidebyside"
#     , leftId = "leftid"
#     , rightId = "rightid"
#   ) %>%
#   addLayersControl(overlayGroups = c("april", "may")) %>%
#   addControl(htmltools::HTML("April 2020"), position = "bottomleft") %>%
#   addControl(htmltools::HTML("May 2020"), position = "bottomright")
# 
# 




###################
# download equity considerations dataset
###################

## ----------- equity considerations data
# HOLC_PYLW	Share of tract's land acreage falling in the yellow ("Definitely Declining") zone of the 1934 Home Owner's Loan Corporation redlining map (Minneapolis and Saint Paul only)
# HOLC_PRED	Share of tract's land acreage falling in the red ("Hazardous") zone of the 1934 Home Owner's Loan Corporation redlining map (Minneapolis and Saint Paul only)

temp <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_equity_considerations/xlsx_society_equity_considerations.zip",
  destfile = temp
)

equity <- readxl::read_xlsx(unzip(temp, "EquityConsiderations_Full.xlsx")) %>%
  janitor::clean_names() 

fs::file_delete("EquityConsiderations_Full.xlsx")


## --------------variables of interest from equity considerations
equity_data_raw <- equity %>%
  select(tr10, 
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
         pownhome) %>%
  rowwise() %>%
  mutate(luse_notgreen = 1 - luse_green,
         pbipoc = 1 - pwhitenh,
         holc_pred = if_else(is.na(holc_pred), 0, holc_pred),
         sens_age = p_0017 + p_65up) %>% #"mutate" reformats any variables that need it
  select(-luse_notgreen,#and then I want to remove the variable I don't need anymore
         -pwhitenh) 


###################
# combine data sources
###################

eva_data_raw <- equity_data_raw %>% 
  full_join(canopy %>% rename(tr10 = GEOID10)) %>%
  full_join(tract_ndvi %>% rename(tr10 = GEOID10)) %>%
  mutate(ndvi2 = ndvi,
         canopy_percent2 = canopy_percent) %>%
  rename(tract_string = tr10) #and for this project, I need to rename the tract variable

###################
# add some human-readable metadata
###################

## -------------------------------describe data
#cc (climate change preset) = prim_flood, avg_temp, ndvi
#ej (environmental justice preset) = pbipoc, phhi_qntl1, prim_flood, avg_temp, ndvi
#ph (public health preset)
eva_data_codes <- tribble(~variable, ~name, ~type, ~interpret_high_value, ~cc, ~ej, ~ph, ~cons,
                          "ppov185",	"% people with income <185% of the poverty threshold", "people", "high_opportunity", 0, 1, 0, 0,
                          "prim_flood", "% developed acres in primary flood zone", "environment", "high_opportunity", 1, 1, 0, 0,
                          "pbipoc", "% people of color", "people", "high_opportunity", 0, 1, 0, 0,
                          "p_0017", "% people age 17 or younger", "people",  "high_opportunity", 0, 0, 0, 0, 
                          "p_65up", "% people age 65 or older", "people",  "high_opportunity", 0, 0, 0,  0,
                          "avg_temp", "Land surface temp on hot summer day", "environment",  "high_opportunity", 1, 1, 1, 0,
                          # "phhi_qntl1", "% households with annual income less than $35,000 (bottom quintile of households)", "people",  "high_opportunity", 0, 1, 0, 0,
                          # "green_roof", "Water holding potential of green roofs on commercial bldgs", "environment",  "high_opportunity", 
                          "env_cancer", "Lifetime cancer risk from air toxics", "people", "high_opportunity", 0, 1, 1,  0,
                          # "luse_notgreen", "% of tract NOT used for green space", "environment", "high_opportunity"
                          "ndvi", "Average greenness (2020 NDVI)", "tree", "low_opportunity", 1, 0, 1,  0,
                          "ndvi2", "Average greenness (2020 NDVI) - for conservation", "tree", "high_opportunity", 0, 0, 0, 1,
                          "tr_ej", "Area of Environmental Justice Concern", "people", "high_opportunity", 0, 1, 0, 0,
                          "holc_pred", "Share of tract's land acreage redlined", "people", "high_opportunity", 0, 1, 0, 0,
                          "canopy_percent", "% tree canopy coverage in 2020", "tree", "low_opportunity", 1, 0, 1, 0,
                          "canopy_percent2", "% tree canopy coverage in 2020 - for conservation", "tree", "high_opportunity", 0, 0, 0, 1,
                          "mdhhincnow", "Median household income, 2015-2019 period (in 2019 dollars)", "people", "low_opportunity", 0, 0, 0, 0,
                          "sens_age", "% people 17 or younger and 65 or older", "people", "high_opportunity", 0,0,1,0,
                          "pd_any", "% people with any disability", "people", "high_opportunity", 0, 0, 0, 0,
                          "pblacknh", "% residents who identify as Black or African American, non-Latino", "people", "high_opportunity", 0, 0, 0, 0,
                          "pasiannh", "% residents who identify as Asian, non-Latino", "people", "high_opportunity", 0, 0, 0, 0,
                          "phisppop", "% residents who identify as Hispanic or Latino", "people", "high_opportunity", 0, 0, 0, 0,
                          "pamindnh", "% residents who identify as Indigenous, non-Latino", "people", "high_opportunity", 0, 0, 0, 0,
                          "pwk_nowork", "% of residents age 16-64 who did not work in past 12 months", "people", "high_opportunity", 0, 0, 0, 0,
                          "pownhome", "% of residents who own their home", "people", "high_opportunity", 0,0,0,0
                          )
eva_data_codes %>% filter(cons == 1)

###################
# #create final dataset - no spatial data here
# #note: spatial data should be joined after any summarizing is done to save some computation time
###################

# #long data
eva_data_main <- eva_data_raw %>%
  pivot_longer(names_to = "variable", values_to = "raw_value", -tract_string) %>% #end the code after this line if you just want the reshaped data
  group_by(variable) %>%
  mutate(MEAN = mean(raw_value, na.rm = T),
         SD = sd(raw_value, na.rm = T),
         MIN = min(raw_value, na.rm = T),
         MAX = max(raw_value, na.rm = T),
         COUNT = as.numeric(sum(!is.na(raw_value))),
         z_score = (raw_value - MEAN)/SD) %>%
  
  right_join(eva_data_codes, by = 'variable') %>%
  
  # #we want high opportunity to be a high value, so this reorders those values if needed
  # mutate(opportunity_zscore = case_when(interpret_high_value == "high_opportunity" ~ z_score,
  #                                       interpret_high_value == "low_opportunity" ~ z_score * (-1),
  #                                         TRUE ~ NA_real_)) %>%
  
  #create nominal weights
  mutate(weights_nominal = case_when(interpret_high_value == "high_opportunity" ~ (raw_value - MIN) / (MAX - MIN) * 10,
                                     interpret_high_value == "low_opportunity" ~ 10 - (raw_value - MIN) / (MAX - MIN) * 10,
                                     TRUE ~ NA_real_)) %>%
  
  #Weights Standard Score
  mutate(weights_scaled = case_when(interpret_high_value == "high_opportunity" ~ pnorm(z_score) * 10,
                                    interpret_high_value == "low_opportunity" ~ (10 - pnorm(z_score) * 10),
                                    TRUE ~ NA_real_)) %>%
  
  #weights rank
  mutate(weights_rank = case_when(interpret_high_value == "high_opportunity" ~ min_rank(desc(weights_nominal)) / COUNT * 10,
                                  interpret_high_value == "low_opportunity" ~ min_rank(desc(weights_nominal)) / COUNT * 10,
                                  TRUE ~ NA_real_)) %>%
  
  # #rank
  mutate(overall_rank = case_when(interpret_high_value == "high_opportunity" ~ min_rank(desc(as.numeric(weights_nominal))),
                                  interpret_high_value == "low_opportunity" ~ min_rank(desc(as.numeric(weights_nominal))))) %>%
  # 
  #clean
  select(-MEAN, -SD, -MIN, -MAX) 

########
# save data
########

#this works if you're in a package
usethis::use_data(eva_data_main, overwrite = TRUE)

#otherwise use this
# write_csv(eva_data_main, "./eva_data_main.csv")

# ########
# # create metadata
# #########
md1 <- eva_data_main %>% group_by(variable) %>% summarise(MEANRAW = mean(raw_value, na.rm = T),
                                                          MEANSCALED = mean(weights_scaled, na.rm = T))
metadata <- eva_data_main %>%
  dplyr::group_by(type, name, variable, interpret_high_value, cc, ej, ph, cons) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  full_join(md1)

usethis::use_data(metadata, overwrite = TRUE)



#####
# create list of cities
####
ctus <- levels(as.factor(equity$ctu_prmry)) #%>% as_tibble()
usethis::use_data(ctus, overwrite = TRUE)

