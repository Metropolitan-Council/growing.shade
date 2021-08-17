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



###################
# Get ndvi at tract level
##################
# https://code.earthengine.google.com/?scriptPath=users%2Fehe%2FAkanaNDVI%3AMetCouncil%2Flandsat%20%2B%20sentinel%20NDVI%20annual%20maxs%20over%20geo

#pull in sentinel data
tract_ndvi <- read_csv("./data-raw/NDVI_metc_tracts_s2_2020.csv") %>%
  mutate(GEOID10 = as.character(GEOID10)) %>%
  select(-`system:index`, -.geo, -Year)


###################
# download data sources of interest and select relevant variables
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
         holc_pred) %>%
  rowwise() %>%
  mutate(luse_notgreen = 1 - luse_green,
         pbipoc = 1 - pwhitenh) %>% #"mutate" reformats any variables that need it
  select(-luse_notgreen,#and then I want to remove the variable I don't need anymore
         -pwhitenh) 


###################
# combine data sources
###################

eva_data_raw <- equity_data_raw %>% 
  full_join(tract_ndvi %>% rename(tr10 = GEOID10)) %>%
  mutate(ndvi2 = ndvi) %>%
  rename(tract_string = tr10) #and for this project, I need to rename the tract variable

###################
# add some human-readable metadata
###################

## -------------------------------describe data
#cc (climate change preset) = prim_flood, avg_temp, ndvi
#ej (environmental justice preset) = pbipoc, phhi_qntl1, prim_flood, avg_temp, ndvi
#ph (public health preset)
eva_data_codes <- tribble(~variable, ~name, ~type, ~interpret_high_value, ~cc, ~ej, ~ph, ~cons,
                          "ppov185",	"% people whose family income is <185% of the federal poverty threshold", "people", "high_opportunity", 0, 0, 0, 0,
                          "prim_flood", "% developed acres in primary flood zone", "environment", "high_opportunity", 1, 1, 0, 0,
                          "pbipoc", "% people of color", "people", "high_opportunity", 0, 1, 0, 0,
                          "p_0017", "% people age 17 or younger", "people",  "high_opportunity", 0, 0, 1, 0, 
                          "p_65up", "% people age 65 or older", "people",  "high_opportunity", 0, 0, 1,  0,
                          "avg_temp", "Land surface temp on hot summer day", "environment",  "high_opportunity", 1, 1, 1, 0,
                          "phhi_qntl1", "% households with annual income less than $35,000 (bottom quintile of households)", "people",  "high_opportunity", 0, 1, 0, 0,
                          # "green_roof", "Water holding potential of green roofs on commercial bldgs", "environment",  "high_opportunity", 
                          "env_cancer", "Lifetime cancer risk from air toxics", "people", "high_opportunity", 0, 1, 1,  0,
                          # "luse_notgreen", "% of tract NOT used for green space", "environment", "high_opportunity"
                          "ndvi", "Average greenness (tract avg. of max NDVI in 2020)", "tree", "low_opportunity", 1, 0, 1,  0,
                          "ndvi2", "Average greenness (tract avg. of max NDVI in 2020) INVERSE", "tree", "high_opportunity", 1, 0, 1, 1,
                          "tr_ej", "Area of Environmental Justice Concern", "people", "high_opportunity", 0, 1, 0, 0,
                          "holc_pred", "Share of tract's land acreage redlined (Minneapolis and Saint Paul only)", "people", "high_opportunity", 0, 1, 0, 0
                          )

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
  mutate(weights_rank = case_when(interpret_high_value == "high_opportunity" ~ min_rank((weights_nominal)) / COUNT * 10,
                                  interpret_high_value == "low_opportunity" ~ min_rank(desc(weights_nominal)) / COUNT * 10,
                                  TRUE ~ NA_real_)) %>%
  
  # #rank
  mutate(overall_rank = case_when(interpret_high_value == "high_opportunity" ~ min_rank(as.numeric(weights_nominal)),
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
metadata <- eva_data_main %>%
  dplyr::group_by(type, name, variable, interpret_high_value, cc, ej, ph, cons) %>%
  dplyr::count() %>%
  dplyr::ungroup()

usethis::use_data(metadata, overwrite = TRUE)



#####
# create list of cities
####
ctus <- levels(as.factor(equity$ctu_prmry)) #%>% as_tibble()
usethis::use_data(ctus, overwrite = TRUE)
