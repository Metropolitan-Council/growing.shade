#' map_base UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_base_ui <- function(id) {
  ns <- NS(id)
  tagList()
  # use_waiter()
}

#' map_base Server Function
#'
#' @noRd
mod_map_base_server <- function(input, output, session) {
  ns <- session$ns
  # w <- Waiter$new()#, html="Please wait")#, hide_on_render=T)


  output$ns <- renderLeaflet(quoted = TRUE, {
    leaflet() %>%
      setView(
        lat = 44.963, #st_coordinates(map_centroid)[2], #,
        lng = -93.22, #st_coordinates(map_centroid)[1], #,
        zoom = 10
      ) %>%
      addMapPane(name = "Stamen Toner", zIndex = 430) %>%
      addProviderTiles("Stamen.TonerLines",
                       group = "Stamen Toner"
      ) %>%
      addProviderTiles("Stamen.TonerLabels", 
                       options = leafletOptions(pane = "Stamen Toner"),
                       group = "Stamen Toner") %>%
      
      addMapPane(name = "Carto Positron", zIndex = 430) %>%
      addProviderTiles("CartoDB.PositronOnlyLabels", 
                       options = leafletOptions(pane = "Carto Positron"),
                       group = "Carto Positron") %>%
      addProviderTiles("CartoDB.PositronNoLabels",
                       group = "Carto Positron"
      ) %>%
      addProviderTiles(
        provider = providers$Esri.WorldImagery,
        group = "Aerial Imagery"
      ) %>%
      
      #### regional specific other data layers
      addMapPane("trans", zIndex = 431) %>%
      addCircles(
        # Markers(
        data = trans_stops,
        group = "Active transit stops",
        radius = 20,
        fill = T,
        stroke = TRUE,
        weight = 2,
        color = "black", #councilR::colors$transitRed,
        fillColor = "black",# councilR::colors$transitRed,
        options = pathOptions(pane = "trans")
      ) %>%
      # groupOptions(
      #   group = "Active transit stops",
      #   zoomLevels = 13:20
      # ) %>%
      
      # addMapPane("riverlake", zIndex = 429) %>%
      # addPolygons(
      #   data = planting.shade::river_lake,
      #   group = "Rivers & Lakes",
      #   stroke = FALSE,
      #   smoothFactor = 1,
    #   # weight = 0.5,
    #   color = "black",
    #   fill = TRUE,
    #   fillColor = "black",
    #   fillOpacity = 0.9,
    #   options = pathOptions(pane = "riverlake")
    # ) %>%
    
    hideGroup("Active transit stops") %>%
      
      ### add layer control
      addLayersControl(
        position = "bottomright",
        # overlayGroups = c(),
        baseGroups = c(
          "Carto Positron",
          "Stamen Toner",
          "Aerial Imagery"
        ),
        overlayGroups = c(
          "Scores",
          # "Rivers & Lakes",
          "Active transit stops"
        ),
        options = layersControlOptions(collapsed = T)
      ) %>%
      hideGroup(c("Transit"#, 
                  # "Rivers & Lakes"
      ))

  })
}

## To be copied in the UI
# mod_map_base_ui("map_base_ui_1")

## To be copied in the server
# callModule(mod_map_base_server, "map_base_ui_1")
