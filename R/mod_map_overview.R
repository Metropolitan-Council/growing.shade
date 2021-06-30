#' map_overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_map_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"), height = 600)#, width = '80%')#,
    
    # wellPanel(textOutput(ns("selected_tract")))
    
  )
}
    
#' map_overview Server Function
#'
#' @noRd 
mod_map_overview_server <- function(input, output, session,
                                    map_selections,
                                    map_util#,
                                    # current_tab
                                    ){
  ns <- session$ns
  
  output$map <- renderLeaflet({ #  map --------
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
                  )) %>%
      leaflet.extras::addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE)
  })
  
  
  # #printing this works, but unclear how to save it....  
  # map_layer_selection <- reactiveValues()
  # observe({
  #   selected_groups <- req(input$map_groups)
  #   # print(selected_groups)
  #   return(selected_groups)
  #   })
  
  #leaflet observe events -----------
  
  # this stackoverflow was helpful: https://stackoverflow.com/questions/47465896/having-trouble-with-leafletproxy-observeevent-and-shiny-leaflet-application
  
  toListen_mainleaflet <- reactive({
    list(
      # current_tab,
      map_util$map_data2,
      map_selections$goButton
    )
  })
  
  observeEvent(toListen_mainleaflet(),
               {
                 if (is.null(map_util$map_data2)) {
                   print('nodata')
                 } else {
                   print("rendering polygons")
                   leafletProxy("map") %>%
                     clearGroup("score") %>%
                     addMapPane("score", zIndex = 400) %>%
                     addPolygons(
                       data = map_util$map_data2 %>% st_transform(4326),
                       group = "Scores",
                       stroke = TRUE,
                       color =  councilR::colors$suppGray,
                       opacity = 0.9,
                       weight = 0.5, #0.25,
                       fillOpacity = 0.7,
                       smoothFactor = 0.2,
                       highlightOptions = highlightOptions(
                         stroke = TRUE,
                         color = "white",
                         weight = 6,
                         bringToFront = TRUE,
                         opacity = 1
                       ),
                       fillColor = ~ colorNumeric(
                         n = 5,
                         palette = "magma",
                         domain = map_util$map_data2 %>% select("MEAN") %>% .[[1]]
                       )(map_util$map_data2 %>% select("MEAN") %>% .[[1]]),
                       popup = ~paste0("Tract ID: ", map_util$map_data2$tract_string, 
                                       "<br>Average score: ", round(map_util$map_data2$MEAN, 3),
                                       "<br>Rank of score: ", map_util$map_data2$RANK, " out of ", nrow(map_util$map_data2)),
                       options = pathOptions(pane = "score"),
                       layerId = ~tract_string
                     ) %>%
                     # maybe want to add this: https://stackoverflow.com/questions/42245302/shiny-leaflet-highlight-polygon
                     
                     addLegend(
                       # labFormat = labelFormat2(),#labelFormat(prefix = "(", suffix = ")", digits = 5),
                       title = "Average scores",
                       position = "topright",
                       group = "Scores",
                       layerId = "Scores",
                       pal = colorNumeric(
                         n = 5,
                         palette = "magma",
                         domain = map_util$map_data2 %>% select("MEAN") %>% .[[1]]
                       ),
                       values = (map_util$map_data2 %>% select("MEAN") %>% .[[1]])
                     ) 
                     
                   
                 }
               })

  #leaflet print geoid -----------
  
  #ideally want to do nested reactive values?!?
  #https://rtask.thinkr.fr/communication-between-modules-and-its-whims/
  #but this is not working out well for me right now....
  # r <- reactiveValues(test = reactiveValues())
  # observe({
  #   event <- input$map_shape_click
  #   r$test$selected_tract <- (tractoutline$GEOID[tractoutline$GEOID == event$id])
  # })
  # # return(selected_tract)
  
  # #this works, but want to save it
  # observe({
  #   event <- input$map_shape_click
  #   output$selected_tract <- renderText(map_util$map_data2$tract_string[map_util$map_data2$tract_string] == event$id)#renderText(tractoutline$GEOID[tractoutline$GEOID == event$id])
  # })

  #save the selected tract
  vals <- reactiveValues()
  observe({
    event <- input$map_shape_click
    vals$selected_tract <- (map_util$map_data2$tract_string[map_util$map_data2$tract_string == event$id])
  })
  
  return(vals)
 
}
    
## To be copied in the UI
# mod_map_overview_ui("map_overview_ui_1")
    
## To be copied in the server
# callModule(mod_map_overview_server, "map_overview_ui_1")
 

