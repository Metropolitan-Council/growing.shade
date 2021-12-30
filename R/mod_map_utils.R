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

  # we need to make this data for a bar plot, or something like that
  make_plot_data2 <- reactive({
    p <- bg_growingshade_main %>%
      filter(name %in% map_selections$allInputs$value)
    return(p)
  })


  # we need to make this data for the popup
  make_ccp <- reactive({
    p <- bg_growingshade_main %>%
      filter(variable == "canopy_percent")
    return(p)
  })

  # but we want to get a single averaged value for every tract to put on the map
  make_map_data2 <- reactive({
    step1 <- if (map_selections$preset == "Climate change") {
      bg_growingshade_main %>%
        filter(name %in% metadata$name[metadata$cc == 1])
    } else if (map_selections$preset == "Conservation") {
      bg_growingshade_main %>%
        filter(name %in% metadata$name[metadata$cons == 1])
    } else if (map_selections$preset == "Environmental justice") {
      bg_growingshade_main %>%
        filter(name %in% metadata$name[metadata$ej == 1])
    } else if (map_selections$preset == "Public health") {
      bg_growingshade_main %>%
        filter(name %in% metadata$name[metadata$ph == 1])
    } else if (map_selections$preset == "Custom") {
      bg_growingshade_main %>%
        filter(name %in% map_selections$allInputs$value)
      
      # bg_growingshade_main %>%
      #   filter(name %in% c("% tree canopy coverage in 2020"))
    }

    # step1 <- bg_growingshade_main %>%
    #   if (map_selections$preset == "Climate change") {
    #     filter(name %in% metadata$name[metadata$cc == 1])
    #     } else if (map_selections$preset == "Conservation") {
    #       filter(name %in% metadata$name[metadata$cons == 1])
    #       } else if (map_selections$preset == "Environmental justice") {
    #         filter(name %in% metadata$name[metadata$ej == 1])
    #         } else if (map_selections$preset == "Public health") {
    #           filter(name %in% metadata$name[metadata$ph == 1])
    #           } else if (map_selections$preset == "Custom") {
    #             filter(name %in% map_selections$allInputs$value)}


    step2 <- step1 %>%
      group_by(tract_string) %>%
      summarise(MEAN = round(mean(weights_scaled, na.rm = T), 3)) %>%
      mutate(RANK = min_rank(desc(MEAN))) %>%
      left_join(mn_bgs, by = c("tract_string" = "GEOID")) %>%
      st_as_sf() # %>%
    # st_transform(4326)

    # #80
    # profvis::profvis(
    #   data.table::data.table(bg_growingshade_main)[, .(MEAN = mean(weights_scaled, na.rm = T)), tract_string] %>%
    #     mutate(RANK = min_rank(desc(MEAN))) %>%
    #     # .[, RANK:=min_rank(desc(MEAN))] %>%
    #     left_join(mn_bgs, by = c("tract_string" = "GEOID")) #%>%
    #     # st_as_sf() %>%
    #     # st_transform(4326)
    # )


    return(step2)
  })


  #------- reactive things

  vals <- reactiveValues()

  observe({
    vals$plot_data2 <- make_plot_data2()
  })

  observe({
    vals$map_data2 <- make_map_data2()
  })

  observe({
    vals$canopycov <- make_ccp()
  })

  return(vals)
}

## To be copied in the UI
# mod_map_utils_ui("map_utils_ui_1")

## To be copied in the server
# callModule(mod_map_utils_server, "map_utils_ui_1")
