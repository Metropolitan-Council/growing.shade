# export tree canopy block group data
pkgload::load_all()

bgs_table <- mn_bgs %>% 
  sf::st_drop_geometry()

write.csv(bgs_table, "data-raw/exports/minnesota_tree_canopy_block_groups.csv",
          row.names = FALSE)
openxlsx::write.xlsx(bgs_table, "data-raw/exports/minnesota_tree_canopy_block_groups.xlsx")
