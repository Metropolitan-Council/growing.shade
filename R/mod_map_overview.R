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

    leafletOutput(ns("map"), 
                  # width="100%", height="100%"
                  height = "90vh")#,
    
    # wellPanel(textOutput(ns("selected_tract")))
    
  )
}
    
#' map_overview Server Function
#'
#' @noRd 
mod_map_overview_server <- function(input, output, session,
                                    map_selections,
                                    geo_selections,
                                    map_util,
                                    current_tab
                                    ){
  ns <- session$ns

  output$map <- renderLeaflet({ #  map --------
    leaflet() %>%
      setView(
        lat = 44.963,
        lng = -93.32,
        zoom = 10
      ) %>%
      addMapPane(name = "Stamen Toner", zIndex = 100) %>%
      addMapPane(name = "Map", zIndex = 100) %>%
      addMapPane(name = "Aerial Imagery", zIndex = 100) %>%
      addMapPane(name = "Satellite", zIndex = 100) %>%
      addMapPane("redline2", zIndex = 110) %>%
      
      addMapPane(name = "geooutline", zIndex = 149) %>%
      addMapPane("Priority score", zIndex = 150) %>%
      addMapPane("Water", zIndex = 151) %>%
      addMapPane(name = "Road outlines", zIndex = 151) %>%
      addMapPane(name = "geooutline2", zIndex = 152) %>%
      addMapPane("redline", zIndex = 160) %>%

      addMapPane("trans", zIndex = 400) %>%
      addMapPane("EAB", zIndex = 400) %>%
      addMapPane("outline", zIndex = 650) %>%
      addMapPane("labels", zIndex = 651) %>%
      
      
      # leaflet.multiopacity::addOpacityControls(layerId = c(
      #   "Trees",
      #   "score"
      # ),
      # collapsed = T, position = "bottomright"#,
      # # title = "<strong>Opacity control</strong>",
      # # renderOnLayerAdd = TRUE
      # ) %>%
    
      #  #aerial with roads
      addProviderTiles("Stamen.TonerLines", #this is GOOD, but less into it
                       options = c(providerTileOptions(maxZoom=18),
                                   pathOptions(pane = "Road outlines")),
                       group = "Road outlines") %>% #Satellite") %>%
      addProviderTiles("Stamen.TonerLabels",
                       options = c(providerTileOptions(maxZoom=18),
                                   pathOptions(pane = "labels")),# pathOptions(pane = "Stamen Toner"),
                       group = "Satellite") %>%
      addProviderTiles("Stamen.TonerLabels",
                       options = providerTileOptions(maxZoom=18),# pathOptions(pane = "Stamen Toner"),
                       group = "Map") %>%
      addProviderTiles(
        provider = providers$Esri.WorldImagery,
        group = "Satellite",
        options = pathOptions(pane = "Aerial Imagery")
      ) %>%
      addProviderTiles("CartoDB.PositronNoLabels",
                       group = "Map",
                       options = pathOptions(pane = "Map")
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
        label = "EAB infested tree"
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
      
      #maybe not the best, but need city outlines to show up first
      addPolygons(
        data = ctu_list,
        group = "Jurisdiction outlines",
        stroke = T,
        smoothFactor = 1,
        # weight = 1,
        color = "black", ##ED1B2E",
        fillColor = "transparent", #"#ED1B2E",
        fillOpacity = 1,
        options = pathOptions(pane = "geooutline"),
        layerId = ~GEO_NAME
      ) %>%
      addPolygons(
        data = ctu_list,
        group = "Jurisdiction outlines",
        stroke = T,
        smoothFactor = 1,
        # weight = 1,
        color = "black", ##ED1B2E",
        fill = F,
        # fillColor = "transparent", #"#ED1B2E",
        opacity = 1,
        options = pathOptions(pane = "geooutline2"),
        layerId = ~GEO_NAME
      ) %>%
      hideGroup("Active transit stops") %>%
      
      ### add layer control
      addLayersControl(
        position = "bottomright",
        baseGroups = c(
          "Map",
          "Satellite"
        ),
        overlayGroups = c(
          "Priority score",
          "Trees",
          "Water",
          "Active transit stops",
          "Road outlines",
          "Historically redlined areas",
          "Emerald ash borer",
          "Jurisdiction outlines"#,
        ),
        options = layersControlOptions(collapsed = T)
      ) %>% 
      hideGroup(c("Transit",
                  "Emerald ash borer",
                  "Historically redlined areas",
                  "Road outlines")) #%>%
      # groupOptions(
      #       group = "Road outlines",
      #       zoomLevels = 13:13# 13:18
      #     )
  })
    
    #this is GOOD, but I'm less into it now...
    #   groupOptions(
    #     group = "Map",
    #     zoomLevels = 1:12
    #   ) %>%
    # groupOptions(
    #     group = "Satellite",
    #     zoomLevels = 13:18
    #   ) #%>%

  
  #leaflet observe events -----------
  
  toListen_mainleaflet <- reactive({
    list(
      current_tab,
      map_selections$priority_layer, 
      map_util$map_data2
    )
  })
  
  observeEvent(ignoreInit = TRUE,
               toListen_mainleaflet(),
               {
                 if (is.null(map_util$map_data2)) {
                   print('nodata')
                 } else if (map_selections$priority_layer == "Off") {
                   leafletProxy("map") %>%
                     clearGroup("Priority score") %>% 
                     clearControls()
                 } else {
                   print("rendering polygons")
                   leafletProxy("map") %>%
                     clearGroup("Priority score") %>%
                     addPolygons(
                       data = map_util$map_data2 %>% st_transform(4326),
                       group = "Priority score",
                       stroke = TRUE,
                       color =  councilR::colors$suppGray,
                       opacity = 0.9,
                       weight = 0.5, #0.25,
                       fillOpacity = 0.6,
                       smoothFactor = 0.2,
                       label = ~(paste0("Priority score: ", round(map_util$map_data2$MEAN, 3))), 
                       highlightOptions = highlightOptions(
                         stroke = TRUE,
                         color = "white",
                         weight = 6,
                         bringToFront = T,
                         opacity = 1
                       ),
                       fillColor = ~ colorNumeric(
                         n = 5,
                         palette = "Oranges",
                         domain = map_util$map_data2 %>% select("MEAN") %>% .[[1]]
                       )(map_util$map_data2 %>% select("MEAN") %>% .[[1]]),
                       # popup = ~paste0("Tract ID: ", map_util$map_data2$tract_string, 
                       #                 "<br>Priority score: ", round(map_util$map_data2$MEAN, 3),
                       #                 "<br>Rank of score: ", map_util$map_data2$RANK, " out of ", nrow(map_util$map_data2),
                       #                 "<br>Current tree canopy cover: ", round(map_util$canopycov$raw_value, 1)*100, "%"),
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
                         palette = "Oranges",
                         domain = map_util$map_data2 %>% select("MEAN") %>% .[[1]]
                       ),
                       values = (map_util$map_data2 %>% select("MEAN") %>% .[[1]])
                     ) %>%
                     
                     addScaleBar(position = "bottomleft",
                                 options = c(#maxWidth = 200, 
                                             imperial = T, metric = F))
                 }
               })
  
  # map click doesn't work so well with multiple geo options; ctu/tracts/neighborhoods
  
  toListen_clickytracts <- reactive({
    list(
      req(geo_selections$selected_geo == 'tracts'),
      input$map_shape_click$id
    )
  })
  observeEvent(ignoreInit = TRUE,
               # req(geo_selections$selected_geo == 'tracts'),
               # input$map_shape_click$id,
               toListen_clickytracts(), 
               { if (input$map_shape_click$id == "") {
                 leafletProxy("map") %>%
                   clearGroup("Trees") %>%
                   clearGroup("outline") %>%
                   clearGroup("Water")
               } else {
                 leafletProxy("map") %>%
                   clearGroup("Trees") %>%
                   clearGroup("outline") %>%
                   clearGroup("Water") %>%
                   addRasterImage(trees %>%
                                    raster::crop(filter(mn_tracts#crop_tract_ctus
                                                        , GEO_NAME == input$map_shape_click$id)),
                                  colors = "#35978f", #pal,
                                  opacity = .7,
                                  layerId = "Trees",
                                  group = "Trees") %>%

                   addPolygons(
                     data =  mn_tracts %>% filter(GEO_NAME == input$map_shape_click$id),
                     stroke = TRUE,
                     color =  "blue",
                     fill = NA,
                     opacity = 1,
                     group = "outline",
                     smoothFactor = 0.2,
                     options = pathOptions(pane = "outline")) %>%

                   addPolygons(data = river_lake %>% st_crop(filter(mn_tracts #crop_tract_ctus
                                                                    , GEO_NAME == input$map_shape_click$id)),
                               color = "black",
                               fillColor = "black",
                               fillOpacity = .9,
                               fill = T,
                               group = "Water",
                               options = pathOptions(pane = "Water"))
               }
               }
  )
  
# switch cities/ neighborhoods
  observeEvent(ignoreInit = FALSE,
               geo_selections$selected_geo,
               {
                 leafletProxy("map") %>%
                     clearGroup("Jurisdiction outlines") %>%
                     clearGroup("Trees") %>%
                     clearGroup("outline") %>%
                     clearGroup("Water") %>%
                     setView(
                       lat = 44.963,
                       lng = -93.32,
                       zoom = 10
                     ) %>%
                     
                     addPolygons(
                     data = if(geo_selections$selected_geo == 'ctus') {ctu_list} else  {nhood_list},
                     group = "Jurisdiction outlines",
                     stroke = T,
                     smoothFactor = 1,
                     # weight = 1,
                     color = "black", 
                     fillColor = "transparent",
                     opacity = 1,
                     options = pathOptions(pane = "geooutline"),
                     layerId = ~GEO_NAME
                   ) %>%
                   addPolygons(
                     data = if(geo_selections$selected_geo == 'ctus') {ctu_list} else {nhood_list},
                     group = "Jurisdiction outlines",
                     stroke = T,
                     smoothFactor = 1,
                     color = "black", 
                     fill = F,
                     opacity = 1,
                     options = pathOptions(pane = "geooutline2"),
                     layerId = ~GEO_NAME
                   )
                 }
  )

  
  # # if user selects a city/nhood
  observeEvent(ignoreInit = TRUE, # TRUE,
               geo_selections$selected_area,
               { if (geo_selections$selected_area == "") {
                 leafletProxy("map") %>%
                   clearGroup("outline") %>%
                   clearGroup("Trees") %>%
                   clearGroup("Water") %>%
                   setView(
                     lat = 44.963,
                     lng = -93.32,
                     zoom = 10
                   )
               } else if (geo_selections$selected_geo == "tracts") {
                 leafletProxy("map") %>%
                   clearGroup("outline") %>%
                   clearGroup("Trees") %>%
                   clearGroup("Water") %>%
                   setView(
                     lat = 44.963,
                     lng = -93.32,
                     zoom = 10
                   )
               } else {
                 leafletProxy("map") %>%
                   clearGroup("outline") %>%
                   clearGroup("Trees") %>%
                   clearGroup("Water") %>%
                   setView(lng = if (geo_selections$selected_geo == "ctus") {
                     ctu_list[ctu_list$GEO_NAME == geo_selections$selected_area, ]$lat
                     } else if (geo_selections$selected_geo == "nhood") {
                       nhood_list[nhood_list$GEO_NAME == geo_selections$selected_area, ]$lat
                     },
                           lat = if (geo_selections$selected_geo == "ctus") {
                             ctu_list[ctu_list$GEO_NAME == geo_selections$selected_area, ]$long
                           } else if (geo_selections$selected_geo == "nhood") {
                             nhood_list[nhood_list$GEO_NAME == geo_selections$selected_area, ]$long
                           },
                           zoom =if (geo_selections$selected_geo == "ctus") {
                             ctu_list[ctu_list$GEO_NAME == geo_selections$selected_area, ]$zoom
                           } else if (geo_selections$selected_geo == "nhood") {
                             nhood_list[nhood_list$GEO_NAME == geo_selections$selected_area, ]$zoom
                           }) %>%
                   addRasterImage(trees %>%
                                    raster::crop(filter(if (geo_selections$selected_geo == "ctus") {
                                      ctu_list} else if (geo_selections$selected_geo == "nhood") {
                                        nhood_list}, GEO_NAME == geo_selections$selected_area)),
                                  colors = "#35978f",
                                  opacity = .7,
                                  layerId = "Trees",
                                  group = "Trees") %>%
                   addPolygons(
                     data =  filter(if (geo_selections$selected_geo == "ctus") {ctu_list} else {nhood_list}, 
                                    GEO_NAME == geo_selections$selected_area),
                     stroke = TRUE,
                     color =  "blue",
                     fill = NA,
                     opacity = 1,
                     group = "outline",
                     smoothFactor = 0.2,
                     options = pathOptions(pane = "outline"))
               }
               }
  )

  ### save map clicks
  
  vals <- reactiveValues()
  observe({
    event <- input$map_shape_click
    vals$selected_tract <- (map_util$map_data2$tract_string[map_util$map_data2$tract_string == event$id])
    # vals$clicked_geo <-  input$map_shape_click$id
  })
  
  return(vals)
 
}
    
## To be copied in the UI
# mod_map_overview_ui("map_overview_ui_1")
    
## To be copied in the server
# callModule(mod_map_overview_server, "map_overview_ui_1")
 

