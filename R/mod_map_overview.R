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
  
  # .leaflet-pane leaflet-map-pane{
  #   transform: translate3d(-6.75px, -71.1px, 0px)'
  #  }
  #  .leaflet-proxy leaflet-zoom-animated{
  #  transform:translate3d(31560px, 4718px, 0px) scale(256);
  #  }
  # }
  tagList(
    tags$style(type = "text/css", "#map {width; 100%;}"), #works
    tags$style(HTML(js_map)),
    #doesnt work
    # tags$style(class = "d-none d-lg-block", #desktop
    #            type = "text/css", "#map {width; 100% !important;}"), # height: calc(100vh - 200px) !important;}"),
    # tags$style(class = "d-block d-lg-none",#mobile
    #            type = "text/css", "#map {width; 95% !important;}"),
    useWaiter(),
    # waiter::useWaitress(color = "blue"),

    leafletOutput(ns("map"),
      # width="100%", height="100%"
      height = "88vh"
    ) # ,

    # wellPanel(textOutput(ns("selected_blockgroup")))
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
    html = waiter::spin_loader(), # spin_fading_circles(),
    color = "rgba(255,255,255,.5)"
  ) # , transparent(alpha = .5))


  #### question ----
  # # if the radio buttons change, can I reset the input????
  # observeEvent(geo_selections$selected_geo, {
  #   updateTextInput(session, geo_selections$selected_area, value = "")
  # })
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
      leaflet.extras::addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE) %>%
      addMapPane(name = "Stamen Toner", zIndex = 100) %>%
      addMapPane(name = "Map", zIndex = 100) %>%
      addMapPane(name = "Aerial Imagery", zIndex = 100) %>%
      addMapPane(name = "Satellite", zIndex = 100) %>%
      addMapPane("redline2", zIndex = 110) %>%
      # addMapPane(name = "geooutline", zIndex = 149) %>%
      addMapPane("Priority score", zIndex = 120) %>%
      # addMapPane("Water", zIndex = 151) %>%
      addMapPane(name = "Road outlines", zIndex = 151) %>%
      addMapPane(name = "geooutline2", zIndex = 152) %>%
      addMapPane("redline", zIndex = 160) %>%
      # addMapPane("trans", zIndex = 200) %>%
      # addMapPane("EAB", zIndex = 201) %>%
      addMapPane("outline", zIndex = 250) %>%
      addMapPane("labels", zIndex = 251) %>%
      addMapPane("Trees", zIndex = 130) %>%
      # leaflet.multiopacity::addOpacityControls(layerId = c(
      #   "Trees",
      #   "score"
      # ),
      # collapsed = T, position = "bottomright"#,
      # # title = "<strong>Opacity control</strong>",
      # # renderOnLayerAdd = TRUE
      # ) %>%

      # add tree tiles
        addTiles(ui_params$set[ui_params$param == "tree_tile_location"],
             attribution = NULL,
        options = c(
          tileOptions(opacity = .6),
          pathOptions(pane = "Trees")
        ),
        group = "Trees"
      ) %>%
      #  #aerial with roads
      addProviderTiles("Stamen.TonerLines", # this is GOOD, but less into it
        options = c(
          providerTileOptions(maxZoom = 18),
          pathOptions(pane = "Road outlines")
        ),
        group = "Road outlines"
      ) %>%
      # Satellite") %>%
      addProviderTiles("Stamen.TonerLabels",
        options = c(
          providerTileOptions(maxZoom = 18),
          pathOptions(pane = "labels")
        ), # pathOptions(pane = "Stamen Toner"),
        group = "Satellite"
      ) %>%
      addProviderTiles("Stamen.TonerLabels",
        options = providerTileOptions(maxZoom = 18), # pathOptions(pane = "Stamen Toner"),
        group = "Map"
      ) %>%
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
      # addCircles(
      #   data = trans_stops,
      #   group = "Active transit stops",
      #   radius = 20,
      #   fill = T,
      #   stroke = TRUE,
      #   weight = 2,
      #   color = "black",
      #   fillColor = "black",
      #   options = pathOptions(pane = "trans")
      # ) %>%
      # addCircles(
      #   data = eab,
      #   group = "Emerald ash borer",
      #   # layerId = NULL,
      #   radius = 15,
      #   fill = T,
      #   stroke = TRUE,
      #   weight = 4,
      #   opacity = 1,
      #   fillOpacity = .8,
      #   color = "#6a3d9a",
      #   fillColor = "white",
      #   options = pathOptions(pane = "EAB"),
      #   label = "EAB infested tree"
      # ) %>%
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
        fillColor = "black", # "#ED1B2E",
        fillOpacity = 1,
        options = pathOptions(pane = "redline2")
      ) %>%
      # maybe not the best, but need city outlines to show up first
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
        data = filter(ctu_list, GEO_NAME == ui_params$set[ui_params$param == "cityselected"]),
        stroke = TRUE,
        color = "#0073e0", # "blue",
        fill = NA,
        opacity = 1,
        group = "outline",
        smoothFactor = 0.2,
        options = pathOptions(pane = "outline")
      ) %>%
      # addPolygons(
      #   data = ctu_list,
      #   group = "Jurisdiction outlines",
      #   stroke = T,
      #   smoothFactor = 1,
      #   color = "black",
      #   fillColor = "transparent",
      #   fillOpacity = 1,
      #   options = pathOptions(pane = "geooutline"),
      #   layerId = ~GEO_NAME
      # ) %>%
      # addPolygons(
      #   data = ctu_list,
      #   group = "Jurisdiction outlines",
      #   stroke = T,
      #   smoothFactor = 1,
      #   color = "black",
      #   fill = F,
      #   opacity = 1,
      #   options = pathOptions(pane = "geooutline2"),
      # ) %>%

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
          # "Water",
          # "Active transit stops",
          "Road outlines",
          "Historically redlined areas",
          # "Emerald ash borer",
          "Jurisdiction outlines" # ,
        ),
        options = layersControlOptions(collapsed = T)
      ) %>%
      hideGroup(c(
        # "Active transit stops",
        # "Emerald ash borer",
        "Historically redlined areas",
        "Road outlines"
      )) #%>%
      # groupOptions(
      #   group = "Trees",
      #   zoomLevels = 15:18
      # )
    # groupOptions(
    #       group = "Road outlines",
    #       zoomLevels = 13:13# 13:18
    #     )
  })

  # this is GOOD, but I'm less into it now...
  #   groupOptions(
  #     group = "Map",
  #     zoomLevels = 1:12
  #   ) %>%
  # groupOptions(
  #     group = "Satellite",
  #     zoomLevels = 13:18
  #   ) #%>%



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
        # } else if (map_selections$priority_layer == "Off") {
        #   leafletProxy("map") %>%
        #     clearGroup("Priority score") %>%
        #     clearControls()
      } else {
        print("rendering polygons")
        waitertest$show()
        leafletProxy("map") %>%
          clearGroup("Priority score") %>%
          addPolygons(
            data = map_util$map_data2,
            group = "Priority score",
            stroke = TRUE,
            color = "#666666", # councilR::colors$suppGray,
            opacity = 0.9,
            weight = 0.5, # 0.25,
            fillOpacity = 0.5,
            smoothFactor = 0.2,
            label = ~ (paste0("Priority score: ", ifelse(!is.na(map_util$map_data2$MEAN), round(map_util$map_data2$MEAN, 3), "NA, this is a non-residential area"))),
            highlightOptions = highlightOptions(
              stroke = TRUE,
              color = "white",
              weight = 6,
              bringToFront = T,
              opacity = 1
            ),
            fillColor = ~ colorNumeric(
              # n = 5,
              palette = "YlOrBr", # "YlOrRd", #"Oranges",
              domain = map_util$map_data2 %>% select("MEAN") %>% .[[1]], na.color = "#fff"
            )(map_util$map_data2 %>% select("MEAN") %>% .[[1]]),
            popup = ~ paste0(
              "Geographic ID: ", map_util$map_data2$bg_string,
              "<br>City: ", map_util$map_data2$jurisdiction,
              "<br>Priority score: ",  ifelse(!is.na(map_util$map_data2$MEAN), round(map_util$map_data2$MEAN, 3), "NA, this is a non-residential area"),
              # "<br>Rank of score: ", map_util$map_data2$RANK, " out of ", nrow(map_util$map_data2),
              "<br>Current tree canopy cover: ", round(map_util$map_data2$canopy_percent * 100, 1), "%"
            ),
            options = pathOptions(pane = "Priority score"),
            layerId = ~bg_string
          ) %>%
          # maybe want to add this: https://stackoverflow.com/questions/42245302/shiny-leaflet-highlight-polygon
          addLegend(
            # labFormat = labelFormat2(),#labelFormat(prefix = "(", suffix = ")", digits = 5),
            title = "Priority scores<br>(10 = highest priority)", # (higher scores show<br>where trees may have<br>larger benefits)",
            position = "bottomleft",
            group = "Priority score",
            layerId = "score",
            pal = colorNumeric(
              # n = 5,
              palette = "YlOrBr", #"YlOrRd", #"Oranges",
              domain = map_util$map_data2 %>% select("MEAN") %>% .[[1]]
              # ,na.color="#03fc13"
            ),
            values = (map_util$map_data2 %>% select("MEAN") %>% .[[1]])
          ) %>%
          addScaleBar(
            position = "bottomleft",
            options = c( # maxWidth = 200,
              imperial = T, metric = F
            )
          )
        waitertest$hide()
      }
    }
  )

  # map click doesn't work so well with multiple geo options; ctu/blockgroups/neighborhoods
  ## jurisdiction outlines -----------
  observeEvent(
    ignoreInit = FALSE, # true
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
          clearGroup("outline") # %>%
      } else if (geo_selections$selected_geo == "ctus") {
        leafletProxy("map") %>%
          clearGroup("outline") %>%
          addPolygons(
            data = filter(ctu_list, GEO_NAME == geo_selections$selected_area),
            stroke = TRUE,
            color = "#0073e0", # "blue",
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
            color = "#0073e0", # "blue",
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
            color = "#0073e0", #"blue",
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
    # vals$clicked_geo <-  input$map_shape_click$id
  })
  return(vals)
}

## To be copied in the UI
# mod_map_overview_ui("map_overview_ui_1")

## To be copied in the server
# callModule(mod_map_overview_server, "map_overview_ui_1")
