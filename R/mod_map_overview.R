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
      addMapPane(name = "Map", zIndex = 100) %>%
      addMapPane(name = "Aerial Imagery", zIndex = 100) %>%
      addMapPane(name = "Satellite", zIndex = 100) %>%
      addMapPane(name = "Road outlines", zIndex = 151) %>%
      addMapPane("redline", zIndex = 160) %>%
      addMapPane("redline2", zIndex = 110) %>%
      addMapPane("trans", zIndex = 400) %>%
      addMapPane("EAB", zIndex = 400) %>%
      addMapPane("outline", zIndex = 650) %>%
      # leaflet.multiopacity::addOpacityControls(layerId = c(
      #   "Trees",
      #   "score"
      # ),
      # collapsed = T, position = "bottomright"#,
      # # title = "<strong>Opacity control</strong>",
      # # renderOnLayerAdd = TRUE
      # ) %>%
    
    
    
      # set max zoom on labels since the aerial imagery is coarser: https://gis.stackexchange.com/questions/301710/r-leaflet-set-zoom-level-of-tiled-basemap-esri-world-imagery

      
      # addProviderTiles("Stamen.TonerLines",
      #                  group = "Stamen Toner"
      # ) %>%
      # addProviderTiles("Stamen.TonerHybrid", #"Stamen.TonerLabels",
      #                  pathOptions(pane = "Stamen Toner"),
      #                  group = "Stamen Toner") %>%
      # addProviderTiles("Stamen.TonerHybrid", #"Stamen.TonerLabels",
      #                  options = c(zIndex = 400),# pathOptions(pane = "Stamen Toner"),
      #                  group = "Stamen Toner") %>%
      
      #  #aerial with roads
      addProviderTiles("Stamen.TonerLines", #this is GOOD, but less into it
                       options = providerTileOptions(pathOptions(pane = "Road outlines"),
                                   maxZoom=18),
                       group = "Road outlines") %>% #Satellite") %>%
      addProviderTiles("Stamen.TonerLabels",
                       options = providerTileOptions(#zIndex = 600,
                                   maxZoom=18),# pathOptions(pane = "Stamen Toner"),
                       group = "Satellite") %>%
      addProviderTiles("Stamen.TonerLabels",
                       options = providerTileOptions(#zIndex = 600,
                                   maxZoom=18),# pathOptions(pane = "Stamen Toner"),
                       group = "Map") %>%
      addProviderTiles(
        provider = providers$Esri.WorldImagery,
        group = "Satellite",
        options = pathOptions(pane = "Aerial Imagery")
      ) %>%
      
      #  # aerial
      # addProviderTiles("CartoDB.PositronOnlyLabels",
      #                  options = c(zIndex = 400,
      #                              maxNativeZoom=18,maxZoom=18),# pathOptions(pane = "Stamen Toner"),
      #                  group = "Aerial Imagery") %>%
      # addProviderTiles("Stamen.TonerLabels",
      #                  options = c(zIndex = 600),# pathOptions(pane = "Stamen Toner"),
      #                  group = "Aerial Imagery") %>%
      
       #carto positron
      # addProviderTiles("CartoDB.PositronOnlyLabels",
      #                  options = c(zIndex = 400),#pathOptions(pane = "Map"),
      #                  group = "Map") %>%
      addProviderTiles("CartoDB.PositronNoLabels",
                       group = "Map",
                       options = pathOptions(pane = "Map")
      ) %>%
      
      
      # addProviderTiles(
      #   provider = providers$Esri.WorldImagery,
      #   group = "Aerial Imagery",
      #   options = pathOptions(pane = "Aerial Imagery")
      # ) %>%

         
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
      
      addCircles(
        # Markers(
        data = eab,
        group = "Emerald ash borer",
        # layerId = NULL,
        radius = 15,
        fill = T,
        stroke = TRUE,
        weight = 4,
        opacity = 1,
        fillOpacity = .8,
        color = "red", #councilR::colors$transitRed,
        fillColor = "white",# councilR::colors$transitRed,
        options = pathOptions(pane = "EAB"),
        label = "Emerald ash borer tree"
        # labelOptions = labelOptions(noHide = TRUE, offset=c(0,-12), textOnly = TRUE)
      ) %>%
    # addAwesomeMarkers(
    #   group = "EAB",
    #   data = eab,
    #   icon = iconeab,
    #   options = pathOptions(pane = "EAB")
    # )  %>%
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
          "Map",
          # "Aerial Imagery",
          "Satellite"#,
          # "Stamen Toner"
        ),
        overlayGroups = c(
          "Priority score",
          # "Rivers & Lakes",
          "Trees",
          "Water",
          "Active transit stops",
          "Road outlines",
          "Historically redlined areas",
          "Emerald ash borer"
        ),
        options = layersControlOptions(collapsed = T)
      ) %>% 
      hideGroup(c("Transit",
                  "Emerald ash borer",
                  "Historically redlined areas",
                  "Road outlines"
                  # "Rivers & Lakes"
                  ))# %>%
    
    #this is GOOD, but I'm less into it now...
    #   groupOptions(
    #     group = "Map",
    #     zoomLevels = 1:12
    #   ) %>%
    # groupOptions(
    #     group = "Satellite",
    #     zoomLevels = 13:18
    #   ) #%>%

      # addOpacityControls(#collapsed = TRUE, 
      #   position = "bottomright", size = "s",
      #                    # layerId = c("Emerald ash borer")
      #                    group = c("Trees"),
      #   renderOnLayerAdd = TRUE
      #                    ) #%>%
      # leaflet.multiopacity::addOpacityControls(layerId = c(
      #   "Trees"
      # ),
      # collapsed = F, position = "bottomright",
      # title = "<strong>Opacity control</strong>",
      # renderOnLayerAdd = TRUE
      # ) 
      
      #%>%
#       addControl(html="<h1 id='zoom'>Zoom</h1>") %>%
#       htmlwidgets::onRender("function(el,x,data){
#        var map=this;
#        var evt = function(e){
#          $('#zoom').html(map.getZoom())
#        };
#        map.on('zoom', evt);
#        }
# ") #%>%
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
                     addMapPane("Water", zIndex = 151) %>%
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
                                       "<br>Rank of score: ", map_util$map_data2$RANK, " out of ", nrow(map_util$map_data2),
                                       "<br>Current tree canopy cover: ", round(map_util$canopycov$raw_value, 1)*100, "%"),
                       options = pathOptions(pane = "Priority score"),
                       layerId = ~tract_string
                     ) %>%
                     # maybe want to add this: https://stackoverflow.com/questions/42245302/shiny-leaflet-highlight-polygon
                     
                     addLegend(
                       # labFormat = labelFormat2(),#labelFormat(prefix = "(", suffix = ")", digits = 5),
                       title = "Priority scores<br>(10 = highest priority)", #(higher scores show<br>where trees may have<br>larger benefits)",
                       position = "bottomleft",
                       group = "Priority score",
                       layerId = "score",
                       pal = colorNumeric(
                         n = 5,
                         palette = "inferno",
                         domain = map_util$map_data2 %>% select("MEAN") %>% .[[1]]
                       ),
                       values = (map_util$map_data2 %>% select("MEAN") %>% .[[1]])
                     ) %>%
                     
                     addScaleBar(position = "bottomleft",
                                 options = c(#maxWidth = 200, 
                                             imperial = T, metric = F))
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
                                  opacity = .7,
                                  layerId = "Trees",
                                  group = "Trees"#,
                                  # project = FALSE)
                   ) %>%      


                   clearGroup("outline") %>%
                   addPolygons(
                     data = mn_tracts %>% filter(GEOID == input$map_shape_click$id),
                     stroke = TRUE,
                     color =  "blue",
                     fill = NA,
                     opacity = 1, #0.25,
                     group = "outline",
                     smoothFactor = 0.2,
                     options = pathOptions(pane = "outline")) %>%

                   clearGroup("Water") %>%
                   addPolygons(data = river_lake %>% st_crop(filter(crop_tract_ctus,
                                                                    GEOID == input$map_shape_click$id)),
                               color = "black",
                               fillColor = "black",
                               fillOpacity = .9,
                               # layerId = "Water",
                               fill = T,
                               group = "Water",
                               options = pathOptions(pane = "Water"))
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
 

