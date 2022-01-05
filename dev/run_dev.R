# Set options here
options(
  shiny.launch.browser = TRUE,
  scipen = 9999,
  warn = -1,
  verbose = FALSE,
  golem.app.prod = FALSE
) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

planting.shade::render_guides()


# Run the application
run_app()
# profvis::profvis({
#   print(
#     planting.shade::run_app()
#   )
# })

# # ggplot(mpg,aes(hwy, class)) + geom_quasirandom(groupOnX=FALSE, varwidth = TRUE)
library(tidyverse); library(ggbeeswarm)

ggplot() +
  ggbeeswarm::geom_quasirandom(aes(x = raw_value, y = type),
                             # position = position_jitter(seed = 1, width = 0, height = .3),
                             groupOnX = F, varwidth = T,
                             cex = 2, #priority = "density",
                             # method = "compactswarm",
                             # corral = "wrap", corral.width = 0.6,
                             fill = councilR::colors$cdGreen,
                             # size = 3, 
                             col = "black", pch = 21,alpha = .8,
                             data = bg_growingshade_main %>% filter(variable == "canopy_percent"),
                             na.rm = T
)


ctu_list %>%
  ggplot() +
  geom_beeswarm(aes(y = canopy_percent, x = 1),
                # cex = 2.5, 
                corral = "gutter", corral.width = 0.1
                # priority = "random",
                # method = "center"
                ) +
  coord_flip()
  ggbeeswarm::geom_beeswarm(
    size = 3, alpha = .3,
    # position = position_jitter(seed = 1, width = 0, height = .3),
    groupOnX = T, varwidth = T,
    cex = 3.5, #priority = "density",
    col = "grey40",
    aes(x = canopy_percent, y = 1),
    na.rm = T
  ) +
  ggbeeswarm::geom_beeswarm(
    size = 1.3, alpha = .3,
    # position = position_jitter(seed = 1, width = 0, height = .3),
    # groupOnX = T,
    cex = 3.5, #priority = "density",
    col = "grey40",
    aes(x = canopy_percent, y = 2),
    na.rm = T
  )

  ggplot() +
    ggbeeswarm::geom_beeswarm(
      size = 3, alpha = .3,
      # position = position_jitter(seed = 1, width = 0, height = .3),
      # groupOnX = F, varwidth = F,
      cex = 3.5, priority = "density",
      col = "grey40",
      aes(y = canopy_percent, x = 1), data = ctu_list,
      na.rm = T
    ) +
  ggbeeswarm::geom_beeswarm(
    size = 1.3, alpha = .3,
    # position = position_jitter(seed = 1, width = 0, height = .3),
    # groupOnX = FALSE, 
    corral = "wrap", corral.width = 0.4, varwidth = F,
    cex = 3.5, priority = "density",
    col = "grey40",
    aes(y = raw_value, x = 2),
    data = bg_growingshade_main %>% filter(variable == "canopy_percent") %>%
      head(400),
    na.rm = T
  )
                                                                                                                 
