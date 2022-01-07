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

# undev <- raster::raster("./data/greenest2020_glu2016_treemask_undev.tif")
# trees <- raster::raster("./data/tree_raster.tif")
# # trees <- raster::raster("./data/tree_raster.tif")
# # trees <- leaflet::projectRasterForLeaflet(raster::raster("./data/tree_raster.tif"), method= "bilinear") #this actually takes a long time, could do for the tiff
#


# actually tooltips do not meet accessibility guidelines
# #  thanks to: https://github.com/ebailey78/shinyBS/pull/70
# radioTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = NULL){
#
#   options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
#   options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
#   bsTag <- shiny::tags$script(shiny::HTML(paste0("
#     $(document).ready(function() {
#       setTimeout(function() {
#         $('input', $('#", id, "')).each(function(){
#           if(this.getAttribute('value') == '", choice, "') {
#             opts = $.extend(", options, ", {html: true});
#             $(this.parentElement).tooltip('destroy');
#             $(this.parentElement).tooltip(opts);
#           }
#         })
#       }, 500)
#     });
#   ")))
#   htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
# }
#
# #consider https://forum.bootstrapstudio.io/t/how-to-change-the-background-color-of-bootstrap-tooltips/7064
