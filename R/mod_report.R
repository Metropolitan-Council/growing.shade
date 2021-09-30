#' report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_report_ui <- function(id){
  ns <- NS(id)
  tagList(

      h3(uiOutput(ns("geoarea"))),
      h4("Overview"),
      uiOutput(ns("para")),
      HTML("<p><section style='font-size:14px !important'>It would be nice to turn this into an info graphic. Like lightbulbs are 'on' for each ctus total energy usage. Turn them off for how much solar can offset? And fill pools? Not sure... Or think about impaired waterbodies for water retention? </section></p>"),
      
      
      # h3("Priority score versus potential"),
      # HTML("<p><section style='font-size:14px !important'>Identifying buildings/lots which have high potential for solar or stormwater retention is important. But so is identifying areas where solar/green roofs would be disproportionately beneficial based on the prioritization layer. This chart starts to show some of that. Not horribly useful, right now for smaller cities/watersheds.</section></p>"),
      # plotOutput(ns("plot1"), "400px", width = "100%"),
      
      h4("Composition of potential by land use"),
      HTML("<p><section style='font-size:14px !important'>Knowing the underlying land use of these surfaces can be helpful in guiding policy. Since both solar potential and stormwater retention are a function of property area (either rooftop or parking lot), this is what the break down of property area looks like. </section></p>"),
      plotOutput(ns("plot2"), "400px", width = "100%"),
      
      h4("Interconnected issues"),
      HTML("<p><section style='font-size:14px !important'>Something about heat, flooding, carbon emissions, equity. Q for ML - do we have kwH for each city? maybe can't do for watershed district, but maybe can? Or is there a way to get the average residential emissions and look at that? </section></p>"),
      
      
      h4("Cost benefit analysis"),
      HTML("<p><section style='font-size:14px !important'>Can this be done? Something about protecting investments...these green infrastructure projects are win-win type of projects. Something for new developments, something for existing developments. </section></p>"),
      
      
      
      h4("Download data for selected area"),
      HTML("<p><section style='font-size:14px !important'>Acess to the raw data is provided below. Please contact us at <someones email> for further assistance. </section></p>"),
      br(),
      downloadButton(ns("dl_data"), "Download"),
      br()
      
    )
    
}
    
#' report Server Functions
#'
#' @noRd 
mod_report_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    output$geoarea <- renderUI({
      ns <- session$ns
      req(geo_selections$selected_area)
      tagList(
        paste0("Custom report for ", geo_selections$selected_area)
      )
    })
    
    
    output$plot2 <- renderPlot({
      
      donutdata <- if (geo_selections$selected_area == "") {
        region %>% #swp %>% sf::st_drop_geometry() %>%
          group_by(LUSE_USE, FEATURE_TY) %>%
          summarise(AREA = sum(total_area, na.rm = T)) %>%
          arrange(LUSE_USE) %>%
          group_by(FEATURE_TY) %>%
          mutate(fraction = AREA / sum(AREA),
                 ymax = cumsum(fraction),
                 ymin = c(0, head(ymax, n = -1)),
                 label_position = (ymax + ymin) / 2,
                 test = 1-(ymax + ymin)/2) %>%
          mutate(fraction = if_else(fraction >= .05, fraction, NA_real_),
                 test = if_else(fraction >= .05, test, NA_real_))
      } else {
        map_util$swp_nogeo %>% #swp %>% sf::st_drop_geometry() %>%
          group_by(LUSE_USE, FEATURE_TY) %>%
          summarise(AREA = sum(AREA, na.rm = T)) %>%
          arrange(LUSE_USE) %>%
          group_by(FEATURE_TY) %>%
          mutate(fraction = AREA / sum(AREA),
                 ymax = cumsum(fraction),
                 ymin = c(0, head(ymax, n = -1)),
                 label_position = (ymax + ymin) / 2,
                 test = 1-(ymax + ymin)/2)%>%
          mutate(fraction = if_else(fraction >= .05, fraction, NA_real_),
                 test = if_else(fraction >= .05, test, NA_real_))}
      
      donutdata %>%
        mutate(FEATURE_TY = if_else(FEATURE_TY == "NONBUILDING", "Surface parking lot", "Building rooftop")) %>%
        ggplot(aes(y = AREA, x = FEATURE_TY, fill = LUSE_USE)) +
        geom_bar(position = "fill", stat = "identity")  +
        scale_y_continuous(labels = scales::percent(c(0, .25, .5, .75, 1))) +
        councilR::council_theme() +
        theme(legend.position = "bottom",
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 14)) +
        ggrepel::geom_label_repel(aes(y=test, label=paste0(round(fraction * 100, 1), "%")), size=6, show.legend = F) +
        scale_fill_manual(values = c(
          "Industrial and Utility" = "#b2df8a", 
          "Institutional" = "#33a02c", 
          "Mixed Use Commercial" = "#fb9a99", 
          "Mixed Use Industrial" = "#e31a1c", 
          "Mixed Use Residential" = "#fdbf6f", 
          "Multifamily" = "#ff7f00", 
          "Office" = "#cab2d6", 
          "Retail and Other Commercial" = "#6a3d9a")) +
        guides(fill = guide_legend(nrow = 4, byrow = T)) +
        # guides(fill = "none") +
        labs(fill = "Land\nuse",
             y = "Percent of potential")+
        coord_flip()
      
      
    })
    
    output$para <- renderUI({
      if (geo_selections$selected_area == "") {
        HTML("Across the seven-county region there is an annual solar potential of ",
             prettyNum(round(sum(region$total_solar_pot)/1e9, 2), big.mark = ","),
             " billion kilowatt-hours. This is enough energy potential to power X for Y years.
           The total regional stormwater retention potential is ", 
             prettyNum(round(sum(region$total_vol_half)/1e6, 2), big.mark = ","),
             "million cubic feet from six-inch green roofs. This is enough potential to do something.
           Select an area to create a customized report. ")
      } else {
        HTML(
          paste0(geo_selections$selected_area,
                 " has an annual solar production potential of ",
                 round(map_util$center$total_solar_pot/1e6, 2), 
                 " million kilowatt-hours (",
                 round(map_util$center$solar_pot_BUILDING/1e6, 2),
                 " from building rooftops and ",
                 round(map_util$center$solar_pot_NONBUILDING/1e6, 2),
                 " from surface lots) across all land uses. <br><br>",
                 
                 "Six-inch deep green roofs have a potential stormwater retention of ",
                 round(map_util$center$total_vol_half/1e3, 2),
                 " thousand cubic feet (", 
                 round(map_util$center$vol_half_BUILDING/1e3, 2), 
                 " from building rooftops and ", 
                 round(map_util$center$vol_half_NONBUILDING/1e3, 2), " from surface lots). ",
                 
                 "This is equivalent to the capacity of ", 
                 round(map_util$center$total_vol_half / 88286.7, 1), " olympic sized swimming pools!",
                 
                 "<br><br><strong>Scroll to read more.</strong>",
                 "<br><br>",
                 ""
          )
        )
      }
    })
    
    output$dl_data <- downloadHandler(
      filename = function() {paste0("SurfaceWithPurpose_", geo_selections$selected_area, "_", Sys.Date(), ".csv")},
      contentType = "text/csv",
      content = function(con) {
        utils::write.csv(
          x = map_util$swp_nogeo %>%
            transmute(`Building address` = ADDRESS,
                      `City or township name` = CTU_NAME,
                      `Zipcode` = ZIP,
                      `Owner name` = OWNER_NAME,
                      `Land use` = LUSE_USE,
                      `Watershed district` = WMO_NAME,
                      `Type of building` = FEATURE_TY,
                      `Building or lot area (square meters)` = AREA,
                      `Solar potential (annual kWh)` = SOLAR_POT,
                      `Stormwater retention potential (cubic feet with 6" of green roof)` = VOL_HALF_F,
                      `Prioritization score` = MEAN),
          file = con,
          row.names = FALSE
        )}
    )
        
        
  })
}
    
## To be copied in the UI
# mod_report_ui("report_ui_1")
    
## To be copied in the server
# mod_report_server("report_ui_1")
