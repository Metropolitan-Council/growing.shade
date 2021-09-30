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
      plotOutput(ns("tree_plot"), "200px", width = "100%") %>%
        shinyhelper::helper(type = "markdown", content = "LineplotHelp", size = "m"),
      
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
               " tracts which intersect its boundary. The distribution of tree canopy across the region is shown below; tracts falling within ", 
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
    
    
    output$tree_plot <- renderPlot({
      req(geo_selections$selected_area)

      canopyplot<- eva_data_main %>%
        filter(variable %in% c("canopy_percent")) %>%
        select(tract_string, variable, raw_value) %>%
        mutate(flag = if_else(tract_string %in% 
                                if (geo_selections$selected_geo == "ctus") {
                                  c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == geo_selections$selected_area, ]$tract_id)
                                  } else {
                                    c(nhood_crosswalk[nhood_crosswalk$GEO_NAME == geo_selections$selected_area, ]$tract_id)
                                  }, "selected", NA_character_))
      plot <- ggplot()+
        ggdist::stat_halfeye(
          data = canopyplot, aes(x = raw_value, y = 1),
          adjust = .5,  width = .6,  .width = 0,  justification = -.6, 
          point_colour = NA) + 
        geom_boxplot(data = canopyplot, aes(x = raw_value, y = 1),
                     width = .75, outlier.shape = NA) +
        councilR::council_theme() +
        theme(panel.grid.minor = element_blank(),
              panel.grid.major.y = element_blank(),
              axis.text.y = element_blank()) +
        geom_point(size = 1.3,alpha = .3,
                   position = position_jitter(seed = 1, width = 0, height = .3),
                   col = "grey40",
                   aes(x = raw_value, y = 1),
                   data = filter(canopyplot, is.na(flag))) +
        labs(y = "", x = "Tree canopy cover (%)") +
        scale_x_continuous(labels = scales::percent_format(accuracy = 1)) + 
        geom_jitter(aes(x = raw_value, y = 1), 
                    position = position_jitter(seed = 1, width = 0, height = .3), 
                   fill = councilR::colors$cdGreen, 
                   size = 5, col = "black", pch = 21, 
                   data = filter(canopyplot, flag == "selected"))
      return(plot)
    })


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
