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
                                                                                       
# library(leaflet); library(tidyverse); library(sf)
# test <- mn_bgs %>%
#   filter(GEOID =="270531044003") %>% st_transform(4326)
# leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = test)
