#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %||% 1
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList

#' @import leaflet
#' @import tibble
#' @import dplyr 
#' @import ggplot2
#' @import sf
#' @import tidyr
#' @import stringr
#' @import cowplot
#' @import councilR
#' @import fmsb
#' @import shinyjs
#' @import shinyWidgets
#' @import DT
#' @import readr
#' @import leaflet.multiopacity
require(magrittr)
load('./data/eva_data_main.rda')

eva_vars <- eva_data_main %>%
  dplyr::group_by(type, name, variable, interpret_high_value, cc, ej, ph) %>%
  dplyr::count() %>%
  dplyr::ungroup()


labelFormat2 <- function(
  prefix = "(", suffix = ")", between = " &ndash; ", digits = 5, big.mark = ",",
  transform = identity
) {
  
  formatNum <- function(x) {
    format(
      round(transform(x), digits), trim = FALSE, scientific = FALSE,
      big.mark = big.mark
    )
  }
  
  function(type, ...) {
    switch(
      type,
      numeric = (function(cuts) {
        paste0(prefix, formatNum(cuts), suffix)
      })(...), # nolint
      bin = (function(cuts) {w
        n <- length(cuts)
        paste0(prefix, formatNum(cuts[-n]), between, formatNum(cuts[-1]), suffix)
      })(...), # nolint
      quantile = (function(cuts, p) {
        n <- length(cuts)
        p <- paste0(round(p * 100), "%")
        cuts <- paste0(formatNum(cuts[-n]), between, formatNum(cuts[-1]))
        # mouse over the legend labels to see the values (quantiles)
        paste0(
          "<span title=\"", cuts, "\">", prefix, p[-n], between, p[-1], suffix,
          "</span>"
        )
      })(...), # nolint
      factor = (function(cuts) {
        paste0(prefix, as.character(transform(cuts)), suffix)
      })(...) # nolint
    )
  }
  
}

icon_ecosystem <- leaflet::awesomeIcons(
  icon = "fa-leaf",
  iconColor = "black",
  library = "fa",
  markerColor = "purple"
)

icon_community <- leaflet::awesomeIcons(
  icon = "fa-user-md",
  iconColor = "black",
  library = "fa",
  markerColor = "blue"
)

icon_cost <- leaflet::awesomeIcons(
  icon = "fa-power-off",
  iconColor = "black",
  library = "fa",
  markerColor = "orange"
)

icon_bus <- leaflet::awesomeIcons(
  icon = "cash",
  # icon = "fa-money-bill",
  iconColor = "black",
  library = "ion",
  markerColor = "green"
)

story_topic_vars <- readxl::read_xlsx("./data/story generator.xlsx",
                                      col_types = "text") %>%
  dplyr::group_by(topic) %>%
  dplyr::count() %>%
  dplyr::ungroup()

# ag <- raster::raster("./data/greenest2020_glu2016_treemask_ag.tif")
ind <- raster::raster("./data/greenest2020_glu2016_treemask_ind.tif")
inst <- raster::raster("./data/greenest2020_glu2016_treemask_inst.tif")
mixed <- raster::raster("./data/greenest2020_glu2016_treemask_mixeduse.tif")
residential <- raster::raster("./data/greenest2020_glu2016_treemask_residential.tif")
parkgc <- raster::raster("./data/greenest2020_glu2016_treemask_golfpark.tif")
retoff <- raster::raster("./data/greenest2020_glu2016_treemask_retail_office.tif")
undev <- raster::raster("./data/greenest2020_glu2016_treemask_undev.tif")
trees <- raster::raster("./data/tree_raster.tif")
# trees <- leaflet::projectRasterForLeaflet(raster::raster("./data/tree_raster.tif"), method= "bilinear") #this actually takes a long time, could do for the tiff
  
