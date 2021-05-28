#` script to process portland data
library(tidyverse)
library(readxl)

#names from sheet 1
eva_vars <- read_xlsx("./data-raw/Portland CommunityMaster.xlsx",
                              sheet = 1) %>%
  rename(variable = Field,
         name = Name) %>%
  filter(!is.na(Order)) %>%
  mutate(interpret_high_value = case_when(Order == "Descending" ~ "high_opportunity",
                                          Order == "Ascending" ~ "low_opportunity")) %>%
  mutate(type = case_when(str_detect(variable, "cbiz") == "TRUE" ~ "business",
                          str_detect(variable, "cppl") == "TRUE" ~ "people",
                          str_detect(variable, "cplc") == "TRUE" ~ "place")) %>%
  select(variable, name, type, interpret_high_value)




#read in sheet 2, the raw data
eva_data_main <- read_xlsx("./data-raw/Portland CommunityMaster.xlsx", #place excel sheet inside the "data-raw" folder
                        sheet = 2, #if there are multiple sheets, read the sheet that contains raw values
                        skip = 5, #the dataset should start with column names. if there are extraneous rows, remove them here
                        na = c("NA") #if there are NAs in the dataset, tell R to read them as such
) %>%
select(-statename, -countyname, -tract) %>% #remove any extraneous columns
  gather("variable", "raw_value", -tract_string) %>%
  right_join(eva_vars, by = "variable") %>%
  group_by(variable) %>%
  mutate(MEAN = mean(raw_value, na.rm = T),
         SD = sd(raw_value, na.rm = T),
         MIN = min(raw_value, na.rm = T),
         MAX = max(raw_value, na.rm = T),
         COUNT = as.numeric(sum(!is.na(raw_value))),
         z_score = (raw_value - MEAN)/SD) %>%

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
  select(-MEAN, -SD, -MIN, -MAX) %>%
  right_join(eva_vars, by = c("variable", "name", "type", "interpret_high_value")) 

# data.frame(head(eva_data_main))
