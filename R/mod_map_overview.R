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
  tagList(        tags$style(type = "text/css", "#map {width; 100% !important; height: calc(100vh - 80px) !important;}"),

    leafletOutput(ns("map"), width="100%", height="100%")#,
    
    # wellPanel(textOutput(ns("selected_tract")))
    
  )
}
    
#' map_overview Server Function
#'
#' @noRd 
mod_map_overview_server <- function(input, output, session,
                                    map_selections,
                                    map_util,
                                    current_tab
                                    ){
  ns <- session$ns
  
  output$map <- renderLeaflet({ #  map --------
    leaflet() %>%
      setView(
        lat = 44.963,
        lng = -93.42,
        zoom = 10
      ) %>%
      addMapPane(name = "Stamen Toner", zIndex = 100) %>%
      addMapPane(name = "Carto Positron", zIndex = 100) %>%
      addMapPane(name = "Aerial Imagery", zIndex = 100) %>%
      addMapPane(name = "Aerial Imagery with roads", zIndex = 100) %>%
      addMapPane(name = "Road outlines", zIndex = 151) %>%
      addMapPane("redline", zIndex = 160) %>%
      addMapPane("redline2", zIndex = 110) %>%
      addMapPane("trans", zIndex = 400) %>%
      addProviderTiles("Stamen.TonerLines",
                       group = "Stamen Toner"
      ) %>%
      addProviderTiles("Stamen.TonerLines", 
                       options = pathOptions(pane = "Road outlines"),
                       group = "Aerial Imagery with roads") %>%
      addProviderTiles("Stamen.TonerLabels",
                       options = c(zIndex = 400),# pathOptions(pane = "Stamen Toner"),
                       group = "Stamen Toner") %>%
      
      # addProviderTiles("Stamen.TonerLabels",
      #                  options = c(zIndex = 600),# pathOptions(pane = "Stamen Toner"),
      #                  group = "Aerial Imagery") %>%
      addProviderTiles("Stamen.TonerLabels",
                       options = c(zIndex = 600),# pathOptions(pane = "Stamen Toner"),
                       group = "Aerial Imagery with roads") %>%
      addProviderTiles("CartoDB.PositronOnlyLabels",
                       options = c(zIndex = 400),# pathOptions(pane = "Stamen Toner"),
                       group = "Aerial Imagery") %>%
      
      addProviderTiles("CartoDB.PositronOnlyLabels", 
                       options = c(zIndex = 400),#pathOptions(pane = "Carto Positron"),
                       group = "Carto Positron") %>%
      addProviderTiles("CartoDB.PositronNoLabels",
                       group = "Carto Positron",
                       options = pathOptions(pane = "Carto Positron")
      ) %>%
      addProviderTiles(
        provider = providers$Esri.WorldImagery,
        group = "Aerial Imagery",
        options = pathOptions(pane = "Aerial Imagery")
      ) %>%
      addProviderTiles(
        provider = providers$Esri.WorldImagery,
        group = "Aerial Imagery with roads",
        options = pathOptions(pane = "Aerial Imagery")
      ) %>%     
      #### regional specific other data layers
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
      
      addPolygons(
        data = redline,
        group = "Historically redlined areas",
        stroke = T,
          smoothFactor = 1,
          # weight = 0.5,
          color = "black",##ED1B2E",
          fill = FALSE,
          fillColor = "#ED1B2E",
          fillOpacity = 1,
        options = pathOptions(pane = "redline")
      ) %>%
      addPolygons(
        data = redline,
        group = "Historically redlined areas",
        stroke = F,
        smoothFactor = 1,
        # weight = 0.5,
        color = "black",##ED1B2E",
        fill = T,
        fillColor = "black", # "#ED1B2E",
        fillOpacity = 1,
        options = pathOptions(pane = "redline2")
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
          "Aerial Imagery",
          "Aerial Imagery with roads",
          "Carto Positron",
          "Stamen Toner"
        ),
        overlayGroups = c(
          "Priority score",
          # "Rivers & Lakes",
          "Trees",
          "Water",
          "Active transit stops",
          "Historically redlined areas"
        ),
        options = layersControlOptions(collapsed = T)
      ) %>% 
      hideGroup(c("Transit",
                  "Historically redlined areas"
                  # "Rivers & Lakes"
                  )) #%>%
      # leaflet.extras::addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE)
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
      current_tab,
      map_util$map_data2
    )
  })
  
  observeEvent(ignoreInit = TRUE,
               toListen_mainleaflet(),
               {
                 if (is.null(map_util$map_data2)) {
                   print('nodata')
                   } else {
                   print("rendering polygons")
                   leafletProxy("map") %>%
                     clearGroup("Priority score") %>%
                     clearGroup("Water") %>%
                     addMapPane("Priority score", zIndex = 150) %>%
                     addPolygons(
                       data = map_util$map_data2 %>% st_transform(4326),
                       group = "Priority score",
                       stroke = TRUE,
                       color =  councilR::colors$suppGray,
                       opacity = 0.9,
                       weight = 0.5, #0.25,
                       fillOpacity = 0.6,
                       smoothFactor = 0.2,
                       highlightOptions = highlightOptions(
                         stroke = TRUE,
                         color = "white",
                         weight = 6,
                         bringToFront = T,
                         opacity = 1
                       ),
                       fillColor = ~ colorNumeric(
                         n = 5,
                         palette = "inferno",
                         domain = map_util$map_data2 %>% select("MEAN") %>% .[[1]]
                       )(map_util$map_data2 %>% select("MEAN") %>% .[[1]]),
                       popup = ~paste0("Tract ID: ", map_util$map_data2$tract_string, 
                                       "<br>Priority score: ", round(map_util$map_data2$MEAN, 3),
                                       "<br>Rank of score: ", map_util$map_data2$RANK, " out of ", nrow(map_util$map_data2)),
                       options = pathOptions(pane = "Priority score"),
                       layerId = ~tract_string
                     ) %>%
                     # maybe want to add this: https://stackoverflow.com/questions/42245302/shiny-leaflet-highlight-polygon
                     
                     addLegend(
                       # labFormat = labelFormat2(),#labelFormat(prefix = "(", suffix = ")", digits = 5),
                       title = "Priority scores<br>(higher scores show<br>where trees may have<br>larger benefits)",
                       position = "bottomleft",
                       group = "Priority score",
                       layerId = "score",
                       pal = colorNumeric(
                         n = 5,
                         palette = "inferno",
                         domain = map_util$map_data2 %>% select("MEAN") %>% .[[1]]
                       ),
                       values = (map_util$map_data2 %>% select("MEAN") %>% .[[1]])
                     ) 
                 }
               })
  
  observeEvent(ignoreInit = TRUE,
               input$map_shape_click$id,
               {
                 # print(input$map_shape_click)
                 leafletProxy("map") %>%
                   addRasterImage(trees %>%
                                    raster::crop(filter(crop_tract_ctus, #mn_tracts, #crop_tract_ctus, 
                                                        GEOID == input$map_shape_click$id)), #"27123031701")),
                                  
                                  colors = "#238b45", #pal,
                                  opacity = .8,
                                  layerId = "Trees",
                                  group = "Trees"#,
                                  # project = FALSE)
                   ) %>%
                   addPolygons(data = river_lake %>% st_crop(filter(crop_tract_ctus,
                                                                    GEOID == input$map_shape_click$id)),
                               color = "black",
                               fillColor = "black",
                               fillOpacity = .9,
                               fill = T,
                               group = "Water")
               }
  )

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
 

