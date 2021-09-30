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
      
      # conditionalPanel(
      #   ns = ns,
      #   condition = "input.geo == 'nhood'",
        downloadButton(ns("dl_report"), "Download this report"),
        # ),
      
      h4("Tree canopy summary"),
      uiOutput(ns("tree_para")),
      
      h4("Priortization summary"),
      h4("Equity analysis"),
      h4("Other considerations"),
      h4("Other resources")
      
      # HTML("<p><section style='font-size:14px !important'>It would be nice to turn this into an info graphic. Like lightbulbs are 'on' for each ctus total energy usage. Turn them off for how much solar can offset? And fill pools? Not sure... Or think about impaired waterbodies for water retention? </section></p>"),
      # 
      # 
      # # h3("Priority score versus potential"),
      # # HTML("<p><section style='font-size:14px !important'>Identifying buildings/lots which have high potential for solar or stormwater retention is important. But so is identifying areas where solar/green roofs would be disproportionately beneficial based on the prioritization layer. This chart starts to show some of that. Not horribly useful, right now for smaller cities/watersheds.</section></p>"),
      # # plotOutput(ns("plot1"), "400px", width = "100%"),
      # 
      # h4("Composition of potential by land use"),
      # HTML("<p><section style='font-size:14px !important'>Knowing the underlying land use of these surfaces can be helpful in guiding policy. Since both solar potential and stormwater retention are a function of property area (either rooftop or parking lot), this is what the break down of property area looks like. </section></p>"),
      # plotOutput(ns("plot2"), "400px", width = "100%"),
      # 
      # h4("Interconnected issues"),
      # HTML("<p><section style='font-size:14px !important'>Something about heat, flooding, carbon emissions, equity. Q for ML - do we have kwH for each city? maybe can't do for watershed district, but maybe can? Or is there a way to get the average residential emissions and look at that? </section></p>"),
      # 
      # 
      # h4("Cost benefit analysis"),
      # HTML("<p><section style='font-size:14px !important'>Can this be done? Something about protecting investments...these green infrastructure projects are win-win type of projects. Something for new developments, something for existing developments. </section></p>"),
      # 
      # 
      # 
      # h4("Download data for selected area"),
      # HTML("<p><section style='font-size:14px !important'>Acess to the raw data is provided below. Please contact us at <someones email> for further assistance. </section></p>"),
      # br(),
      # downloadButton(ns("dl_data"), "Download"),
      # br()
      
    )
    
}
    
#' report Server Functions
#'
#' @noRd 
mod_report_server <- function(id,
                              geo_selections){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    output$geoarea <- renderUI({
      ns <- session$ns
      req(geo_selections$selected_area)
      tagList(
        paste0("Custom report for ", geo_selections$selected_area)
      )
    })
    
    
    # "GEO has an existing tree canopy coverage of approximately X %. In most areas in our region, a tree canopy coverage of 40% (as detected by our methods) leads to the greatest benefits. Some areas in our region are dominated by native tallgrass prairie and have lower tree coverage - this should not be penalized."
    
    output$tree_para <- renderUI({
      ns <- session$ns
      req(geo_selections$selected_area)
      tagList(
        HTML(paste0(geo_selections$selected_area, " has an existing tree canopy which ranges from ",
               if (geo_selections$selected_geo == "ctus") {
                 ctu_list[ctu_list$GEO_NAME == geo_selections$selected_area, ]$min} else {
                   nhood_list[nhood_list$GEO_NAME == geo_selections$selected_area, ]$min
                 }, "% to ",
               if (geo_selections$selected_geo == "ctus") {
                 ctu_list[ctu_list$GEO_NAME == geo_selections$selected_area, ]$max} else {
                   nhood_list[nhood_list$GEO_NAME == geo_selections$selected_area, ]$max
                 }, "% ",
               "across the ",
               if (geo_selections$selected_geo == "ctus") {
                 ctu_list[ctu_list$GEO_NAME == geo_selections$selected_area, ]$ntracts} else {
                   nhood_list[nhood_list$GEO_NAME == geo_selections$selected_area, ]$ntracts
                 },
               " tracts which intersect its boundary. The distribution of tree canopy across the region is shown below, with tracts falling within ", 
               geo_selections$selected_area,
               " highlighted in green.<br><br>",
               " In most areas in our region, a tree canopy coverage of 40% (as detected by our methods) leads to the greatest benefits. Note that native tallgrass prairie occurs throughout our region - lower tree coverage in areas dominated by tallgrass prairie should not be penalized."

               )
        )
        )
    })
    
    # ctu_list$canopy #avg tree canopy
    # $tracts #num of tracts
    # $min #canopy
    # $max #canopy
    
    
    # output$plot2 <- renderPlot({
    #   
    #   donutdata <- if (geo_selections$selected_area == "") {
    #     region %>% #swp %>% sf::st_drop_geometry() %>%
    #       group_by(LUSE_USE, FEATURE_TY) %>%
    #       summarise(AREA = sum(total_area, na.rm = T)) %>%
    #       arrange(LUSE_USE) %>%
    #       group_by(FEATURE_TY) %>%
    #       mutate(fraction = AREA / sum(AREA),
    #              ymax = cumsum(fraction),
    #              ymin = c(0, head(ymax, n = -1)),
    #              label_position = (ymax + ymin) / 2,
    #              test = 1-(ymax + ymin)/2) %>%
    #       mutate(fraction = if_else(fraction >= .05, fraction, NA_real_),
    #              test = if_else(fraction >= .05, test, NA_real_))
    #   } else {
    #     map_util$swp_nogeo %>% #swp %>% sf::st_drop_geometry() %>%
    #       group_by(LUSE_USE, FEATURE_TY) %>%
    #       summarise(AREA = sum(AREA, na.rm = T)) %>%
    #       arrange(LUSE_USE) %>%
    #       group_by(FEATURE_TY) %>%
    #       mutate(fraction = AREA / sum(AREA),
    #              ymax = cumsum(fraction),
    #              ymin = c(0, head(ymax, n = -1)),
    #              label_position = (ymax + ymin) / 2,
    #              test = 1-(ymax + ymin)/2)%>%
    #       mutate(fraction = if_else(fraction >= .05, fraction, NA_real_),
    #              test = if_else(fraction >= .05, test, NA_real_))}
    #   
    #   donutdata %>%
    #     mutate(FEATURE_TY = if_else(FEATURE_TY == "NONBUILDING", "Surface parking lot", "Building rooftop")) %>%
    #     ggplot(aes(y = AREA, x = FEATURE_TY, fill = LUSE_USE)) +
    #     geom_bar(position = "fill", stat = "identity")  +
    #     scale_y_continuous(labels = scales::percent(c(0, .25, .5, .75, 1))) +
    #     councilR::council_theme() +
    #     theme(legend.position = "bottom",
    #           axis.title.y = element_blank(),
    #           axis.title.x = element_blank(),
    #           axis.text.x = element_text(size = 14),
    #           axis.text.y = element_text(size = 14)) +
    #     ggrepel::geom_label_repel(aes(y=test, label=paste0(round(fraction * 100, 1), "%")), size=6, show.legend = F) +
    #     scale_fill_manual(values = c(
    #       "Industrial and Utility" = "#b2df8a", 
    #       "Institutional" = "#33a02c", 
    #       "Mixed Use Commercial" = "#fb9a99", 
    #       "Mixed Use Industrial" = "#e31a1c", 
    #       "Mixed Use Residential" = "#fdbf6f", 
    #       "Multifamily" = "#ff7f00", 
    #       "Office" = "#cab2d6", 
    #       "Retail and Other Commercial" = "#6a3d9a")) +
    #     guides(fill = guide_legend(nrow = 4, byrow = T)) +
    #     # guides(fill = "none") +
    #     labs(fill = "Land\nuse",
    #          y = "Percent of potential")+
    #     coord_flip()
    #   
    #   
    # })
    # 

    output$dl_report <- downloadHandler(
      filename = paste0("GrowingShade_", Sys.Date(), ".html"),
      content = function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        # Set up parameters to pass to Rmd document
        params <- list(selected_tract = tract_selections$selected_tract,
                       selected_geo = input$geo,
                       selected_city = input$cityInput,
                       vars_used = map_selections$preset,
                       priority_score = map_util$map_data2,#round(map_util$map_data2$MEAN, 3),
                       rank_total = nrow(map_util$map_data2),
                       vars_selected = map_selections$allInputs,
                       canopy = map_util$canopycov)
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv()),
                          # output_format = "pdf_document", #"html_document",
                          output_format = "html_document",
                          output_options = list(html_preview = FALSE,
                                                toc = TRUE, 
                                                # theme = "cosmo",
                                                toc_depth = 3,
                                                fig_caption = TRUE
                                                # theme = NULL,
                                                # css = system.file("app/www/style.css", package = 'planting.shade')
                                                
                          ),
        )
      }
    )
        
        
  })
}
    
## To be copied in the UI
# mod_report_ui("report_ui_1")
    
## To be copied in the server
# mod_report_server("report_ui_1")
