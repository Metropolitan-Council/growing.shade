#' map_utils UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_utils_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' map_utils Server Function
#'
#' @noRd
#' @import sf
mod_map_utils_server <- function(input, output, session,
                                 map_selections,
                                 geo_selections) {
  ns <- session$ns

  make_map_data <- reactive({

    faststep <- if (map_selections$preset == "Climate change") {
      mn_bgs %>%
        mutate(
          bg_string = GEOID,
          MEAN = `Climate change`
        )
    } else if (map_selections$preset == "Conservation") {
      mn_bgs %>%
        mutate(
          bg_string = GEOID,
          MEAN = `Conservation`
        )
    } else if (map_selections$preset == "Environmental justice") {
      mn_bgs %>%
        mutate(
          bg_string = GEOID,
          MEAN = `Environmental justice`
        )
    } else if (map_selections$preset == "Public health") {
      mn_bgs %>%
        mutate(
          bg_string = GEOID,
          MEAN = `Public health`
        )
    } else if (map_selections$preset == "Custom") {
      bg_growingshade_main %>%
        filter(name %in% map_selections$allInputs$value) %>%
        group_by(bg_string) %>%
        summarise(MEAN = round(mean(weights_scaled, na.rm = T), 3)) %>%
        # mutate(RANK = min_rank(desc(MEAN))) %>%
        left_join(mn_bgs, by = c("bg_string" = "GEOID")) %>%
        st_as_sf()
    }
    
    return(faststep)

  }) # %>%
  # bindCache(map_selections$preset,
  #           map_selections$allInputs$value)

  make_map_data_filter <- reactive({
    filterstep <- if(geo_selections$mapfilter == "above4") {
      make_map_data() %>%
        filter(MEAN >= 4)
    } else if (geo_selections$mapfilter == "above5") {
      make_map_data() %>%
        filter(MEAN >= 5)
    } else if (geo_selections$mapfilter == "above6") {
      make_map_data() %>%
        filter(MEAN >= 6)
    } else if (geo_selections$mapfilter == "above7") {
      make_map_data() %>%
        filter(MEAN >= 7)
    } else {make_map_data()}
    
    return(filterstep)
    
  })

  #------- reactive things

  vals <- reactiveValues()

  observe({
    vals$map_data <- make_map_data()
    vals$map_data2 <- make_map_data_filter()
  })

  # observe({
  #   vals$canopycov <- make_ccp()
  # })

  return(vals)
}

## To be copied in the UI
# mod_map_utils_ui("map_utils_ui_1")

## To be copied in the server
# callModule(mod_map_utils_server, "map_utils_ui_1")
