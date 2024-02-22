# compare averages
pkgload::load_all()
library(councilR)
source("data-raw/prev_version_location.R")
# at tag "comparison-point"
load(file.path(prev_version_location, "data/bg_growingshade_main.rda"))
old_bg_growingshade_main <- bg_growingshade_main %>% 
  mutate(source = "Previous") %>% 
  filter(variable == "canopy_percent")
  


bg_growingshade_main <- planting.shade::bg_growingshade_main %>% 
  mutate(source = "Update") %>% 
  filter(variable == "canopy_percent")
  

bg_comp <- bg_growingshade_main %>% 
  bind_rows(old_bg_growingshade_main) %>% 
  mutate(source = factor(source,
                         levels = c(
                                    "Update",
                                    "Previous"),
                         ordered = TRUE))

# regional average values
bg_comp %>% 
  group_by(source) %>% 
  summarize(mean = mean(raw_value, na.rm = T),
            median = median(raw_value, na.rm = T),
            min = min(raw_value, na.rm = T),
            max = max(raw_value, na.rm = T))


# ctu aggregation
bg_summary <- bg_comp %>% 
  full_join(ctu_crosswalk, by = c("bg_string" = "bg_id"), 
            relationship = "many-to-many") %>% 
  group_by(GEO_NAME, source) %>% 
  summarise(
    min = round(min(raw_value) * 100, 2),
    max = round(max(raw_value) * 100, 2),
    mean = round(mean(raw_value * 100), 2),
    n_blockgroups = n()
  )

# bg comparison wide
bg_comp_wide <- bg_comp %>% 
  select(-weights_scaled) %>% 
  pivot_wider(names_from = source,
              values_from = raw_value)

ggplot(data = bg_comp) +
  aes(x = raw_value,
      fill = source) +
  geom_density(alpha = 0.7) +
  labs(x = "2021 BG % Tree Canopy",
       title = "Block group % tree canopy distribution") +
  theme_council(use_showtext = TRUE,
                use_manual_font_sizes = FALSE,
                base_size = 16)


ggplot(data = bg_comp) +
  aes(x = raw_value,
      y = source) +
  geom_boxplot(show.legend = FALSE) +
  labs(x = "Block group tree canopy percent",
       y = "",
       title = "Block group % tree canopy distribution") +
  theme_council(use_showtext = TRUE,
                use_manual_font_sizes = FALSE,
                base_size = 16)

ggplot(data = bg_summary) +
  aes(x = `mean`,
      y = source)+
  geom_boxplot() +
  labs(x = "CTU mean tree canopy percent",
       y = "",
       title = "CTU average % tree canopy distribution") +
  theme_council(use_showtext = TRUE,
                use_manual_font_sizes = FALSE,
                base_size = 16)
