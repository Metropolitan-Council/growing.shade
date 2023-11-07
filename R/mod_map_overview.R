#' map_overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import waiter
mod_map_overview_ui <- function(id) {
  ns <- NS(id)
  
  js_map <- "@media (max-width: 765px) {
   /* #map {width: 50% !important;} */
   div#map_overview_ui_1-map {
    width: 95% !important;
    height: 55vh !important;
    visibility: inherit;
    position: relative;
    right: 5em;
    left: 0em;
    bottom:2em;
   }
   .navbar-right{
   float: right !important;
   }
   body{padding-top:15px;}
   }
  }
  "
  tagList(
    tags$style(type = "text/css", "#map {width; 100%;}"), # works
    tags$style(HTML(js_map)),
    useWaiter(),
    
    leafletOutput(ns("map"),
                  height = "88vh"
    ) 
  )
}

#' map_overview Server Function
#'
#' @noRd
#' @import leaflet
mod_map_overview_server <- function(input, output, session,
                                    map_selections,
                                    geo_selections,
                                    map_util,
                                    current_tab) {
  ns <- session$ns
  
  
  waitertest <- Waiter$new(ns("map"),
                           html = waiter::spin_loader(), 
                           color = "rgba(255,255,255,.5)"
  ) 
  
  
  #### main map ---------
  output$map <- renderLeaflet({ #  map --------
    leaflet(options = leafletOptions(
      minZoom = 8, maxZoom = 17,
      attributionControl = FALSE
    )) %>%
      setView(
        lat = ui_params$number[ui_params$param == "center_latitude"],
        lng = ui_params$number[ui_params$param == "center_longitude"],
        zoom = ui_params$number[ui_params$param == "center_zoom"]
      ) %>%
      # add attribution
      leaflet.extras::addFullscreenControl(position = "topleft", 
                                           pseudoFullscreen = TRUE) %>%
      addMapPane(name = "Carto Positron", zIndex = 100) %>%
      addMapPane(name = "Map", zIndex = 100) %>%
      addMapPane(name = "Aerial Imagery", zIndex = 100) %>%
      addMapPane(name = "Satellite", zIndex = 100) %>%
      addMapPane("redline2", zIndex = 110) %>%
      addMapPane("Priority score", zIndex = 120) %>%
      addMapPane(name = "geooutline2", zIndex = 152) %>%
      addMapPane("redline", zIndex = 160) %>%
      addMapPane("outline", zIndex = 250) %>%
      addMapPane("labels", zIndex = 251) %>%
      addMapPane("Trees", zIndex = 130) %>%
      addTiles(ui_params$set[ui_params$param == "tree_tile_location"],
               attribution = NULL,
               options = c(
                 tileOptions(opacity = .6),
                 pathOptions(pane = "Trees")
               ),
               group = "Trees"
      ) %>%
      addProviderTiles(
        providers$CartoDB.PositronOnlyLabels,
        options = c(
          providerTileOptions(maxZoom = 18),
          pathOptions(pane = "labels")
        ), 
        group = "Satellite"
      ) %>%
      addProviderTiles(
        providers$CartoDB.PositronOnlyLabels,
        options = providerTileOptions(maxZoom = 18),
        group = "Map"
      ) %>%
      addProviderTiles(
        provider = providers$Esri.WorldImagery,
        group = "Satellite",
        options = pathOptions(pane = "Aerial Imagery")
      ) %>%
      addProviderTiles(
        providers$CartoDB.PositronNoLabels,
        group = "Map",
        options = pathOptions(pane = "Map")
      ) %>%
      addPolygons(
        data = redline,
        group = "Historically redlined areas",
        stroke = T,
        smoothFactor = 1,
        color = "black",
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
        color = "black",
        fill = T,
        fillColor = "black",
        fillOpacity = 1,
        options = pathOptions(pane = "redline2")
      ) %>%
      addPolygons(
        data = ctu_list,
        group = "Jurisdiction outlines",
        stroke = T,
        smoothFactor = 1,
        color = "black",
        weight = 2,
        fill = F,
        opacity = 1,
        options = pathOptions(pane = "geooutline2"),
        layerId = ~GEO_NAME
      ) %>%
      addPolygons(
        data = filter(ctu_list, 
                      GEO_NAME == ui_params$set[ui_params$param == "cityselected"]),
        stroke = TRUE,
        color = "#0073e0", 
        fill = NA,
        opacity = 1,
        group = "outline",
        smoothFactor = 0.2,
        options = pathOptions(pane = "outline")
      ) %>%
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
          "Historically redlined areas",
          "Jurisdiction outlines" # ,
        ),
        options = layersControlOptions(collapsed = T)
      ) %>%
      hideGroup(
        c(
          "Historically redlined areas",
          "Road outlines"
        )) 
  })
  
  
  #### changing priority score --------------
  toListen_mainleaflet <- reactive({
    list(
      current_tab,
      # map_selections$priority_layer,
      map_util$map_data2
    )
  })
  
  observeEvent(
    ignoreInit = TRUE, # TRUE,
    toListen_mainleaflet(),
    {
      if (is.null(map_util$map_data2)) {
        print("nodata")
      } else {
        print("rendering polygons")
        waitertest$show()
        leafletProxy("map") %>%
          clearGroup("Priority score") %>%
          addPolygons(
            data = map_util$map_data2,
            group = "Priority score",
            stroke = TRUE,
            color = "#666666",
            opacity = 0.9,
            weight = 0.5,
            fillOpacity = 0.5,
            smoothFactor = 0.2,
            label = ~ (paste0("Priority score: ", 
                              ifelse(!is.na(map_util$map_data2$MEAN),
                                     round(map_util$map_data2$MEAN, 2),
                                     "NA, this is a non-residential area"
                              ))),
            highlightOptions = highlightOptions(
              stroke = TRUE,
              color = "white",
              weight = 6,
              bringToFront = T,
              opacity = 1
            ),
            fillColor = ~ colorNumeric(
              palette = "YlOrBr", 
              domain = map_util$map_data2 %>% select("MEAN") %>% .[[1]],
              na.color = "#fff"
            )(map_util$map_data2 %>% select("MEAN") %>% .[[1]]),
            popup = ~ paste0(
              "<b>Geographic ID</b> ", map_util$map_data2$bg_string,
              "<br><b>City</b> ", map_util$map_data2$jurisdiction,
              "<br><b>Priority score</b> ", ifelse(!is.na(map_util$map_data2$MEAN),
                                             round(map_util$map_data2$MEAN, 2),
                                             "NA, this is a non-residential area"
              ),
              "<br><b>Current tree canopy cover</b> ", round(map_util$map_data2$canopy_percent * 100, 1), "%"
            ),
            options = pathOptions(pane = "Priority score"),
            layerId = ~bg_string
          ) %>%
          addLegend(
            title = "Priority scores<br>(10 = highest priority)", 
            position = "bottomleft",
            group = "Priority score",
            layerId = "score",
            pal = colorNumeric(
              palette = "YlOrBr",
              domain = map_util$map_data2 %>% select("MEAN") %>% .[[1]]
            ),
            values = (map_util$map_data2 %>% select("MEAN") %>% .[[1]])
          ) %>%
          addScaleBar(
            position = "bottomleft",
            options = c( 
              imperial = TRUE, metric = FALSE
            )
          )
        waitertest$hide()
      }
    }
  )
  
  ## jurisdiction outlines -----------
  observeEvent(
    ignoreInit = FALSE, 
    geo_selections$selected_geo,
    {
      if (geo_selections$selected_geo == "blockgroups") {
        leafletProxy("map") %>%
          clearGroup("Jurisdiction outlines") %>%
          clearGroup("outline")
      } else {
        leafletProxy("map") %>%
          clearGroup("Jurisdiction outlines") %>%
          clearGroup("outline") %>%
          addPolygons(
            data = if (geo_selections$selected_geo == "ctus") {
              ctu_list
            } else if (geo_selections$selected_geo == "nhood") {
              nhood_list
            } else if (geo_selections$selected_geo == "blockgroups") {
              mn_bgs
            },
            group = "Jurisdiction outlines",
            stroke = T,
            smoothFactor = 1,
            color = "black",
            weight = 2,
            fill = F,
            opacity = 1,
            options = pathOptions(pane = "geooutline2"),
            layerId = if (geo_selections$selected_geo == "blockgroups") {
              NULL
            } else {
              ~GEO_NAME
            }
          )
      }
    }
  )
  
  # trees for city/nhood ------
  
  observeEvent(
    ignoreInit = FALSE, # TRUE,
    req(geo_selections$selected_area != "blockgroups"),
    {
      if (geo_selections$selected_area == "") {
        leafletProxy("map") %>%
          clearGroup("outline") 
      } else if (geo_selections$selected_geo == "ctus") {
        leafletProxy("map") %>%
          clearGroup("outline") %>%
          addPolygons(
            data = filter(ctu_list, GEO_NAME == geo_selections$selected_area),
            stroke = TRUE,
            color = "#0073e0", 
            fill = NA,
            opacity = 1,
            group = "outline",
            smoothFactor = 0.2,
            options = pathOptions(pane = "outline")
          )
      } else if (geo_selections$selected_geo == "nhood") {
        leafletProxy("map") %>%
          clearGroup("outline") %>%
          addPolygons(
            data = filter(nhood_list, GEO_NAME == geo_selections$selected_area),
            stroke = TRUE,
            color = "#0073e0",
            fill = NA,
            opacity = 1,
            group = "outline",
            smoothFactor = 0.2,
            options = pathOptions(pane = "outline")
          )
      }
    }
  )
  
  # # trees for blockgroups  --------------
  toListen_clickyblockgroups <- reactive({
    list(
      req(geo_selections$selected_geo == "blockgroups"),
      req(input$map_shape_click$id)
    )
  })
  observeEvent(
    ignoreInit = FALSE,
    toListen_clickyblockgroups(),
    {
      if (input$map_shape_click$id == "") {
        leafletProxy("map") %>%
          clearGroup("outline")
      } else {
        leafletProxy("map") %>%
          clearGroup("outline") %>%
          addPolygons(
            data = mn_bgs %>% filter(GEO_NAME == input$map_shape_click$id),
            stroke = TRUE,
            color = "#0073e0", 
            fill = NA,
            opacity = 1,
            group = "outline",
            smoothFactor = 0.2,
            options = pathOptions(pane = "outline")
          )
      }
    }
  )
  
  
  # ### save map clicks -----------
  vals <- reactiveValues()
  observe({
    req(geo_selections$selected_geo == "blockgroups")
    event <- input$map_shape_click
    vals$TEST <- event$id
    vals$selected_blockgroup <- (map_util$map_data2$bg_string[map_util$map_data2$bg_string == event$id])
  })
  return(vals)
}

## To be copied in the UI
# mod_map_overview_ui("map_overview_ui_1")

## To be copied in the server
# callModule(mod_map_overview_server, "map_overview_ui_1")
