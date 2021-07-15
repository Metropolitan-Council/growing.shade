#' main_leaflet UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_main_leaflet_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"), width = "100%", height = 700),
  )
}

#' main_leaflet Server Function
#'
#' @noRd
mod_main_leaflet_server <- function(
  input,
  output,
  session,
  map_util,
  map_selections,
  current_tab
) {
  ns <- session$ns

  # output$map ----
  output$map <- mod_map_base_server(
    input = input,
    output = output,
    session = session
  )

  # adding proxys --------

  # add pop polygons -----
  toListen_mainleaflet <- reactive({
    list(
      # current_tab,
      map_util$map_data2,
      map_selections$goButton
    )
  })

  observeEvent(
    toListen_mainleaflet(),
    {
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
  )




  # startup ------------------
  observeEvent(
    once = TRUE,
    ignoreInit = TRUE,
    label = "startup",
    current_tab,
    {
      print("Rendering start-up map")
      


      leafletProxy("map") %>%
      addPolygons(
        data = startdata,
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
        domain = startdata %>% select("MEAN") %>% .[[1]]
      )(startdata %>% select("MEAN") %>% .[[1]]),
      popup = ~paste0("Tract ID: ", startdata$tract_string,
                      "<br>Average score: ", round(startdata$MEAN, 3),
                      "<br>Rank of score: ", startdata$RANK, " out of ", nrow(startdata)),
      # options = pathOptions(pane = "score"),
      layerId = ~tract_string
      ) %>%
        addLegend(
          # labFormat = labelFormat2(),#labelFormat(prefix = "(", suffix = ")", digits = 5),
          title = "Average scores",
          position = "topright",
          group = "Scores",
          layerId = "Scores",
          pal = colorNumeric(
            n = 5,
            palette = "magma",
            domain = startdata %>% select("MEAN") %>% .[[1]]
          ),
          values = (startdata %>% select("MEAN") %>% .[[1]])
        )
    }
  )
}

## To be copied in the UI
# mod_main_leaflet_ui("main_leaflet_ui_1")

## To be copied in the server
# callModule(mod_main_leaflet_server, "main_leaflet_ui_1")
