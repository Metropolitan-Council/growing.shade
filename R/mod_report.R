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
      uiOutput(ns("priority_para")),
      plotOutput(ns("priority_plot"), "400px", width = "100%"), # %>%
        # shinyhelper::helper(type = "markdown", content = "LineplotHelp", size = "m"),

      h4("Equity analysis"),
      uiOutput(ns("equity_para")),
      plotOutput(ns("equity_plot"), "400px", width = "100%"), # %>%
      
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
                              geo_selections,
                              map_selections,
                              map_util){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    output$geoarea <- renderUI({
      ns <- session$ns
      req(geo_selections$selected_area)
      tagList(
        paste0("Custom report for ", geo_selections$selected_area)
      )
    })
    
    
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
               " tracts which intersect its boundary. The distribution of tree canopy across the region is shown below; tracts in ", 
               geo_selections$selected_area,
               " are highlighted in green.<br><br>",
               " In most areas in our region, a tree canopy coverage of 40% (as detected by our methods) leads to the greatest benefits. Note that native tallgrass prairie occurs throughout our region - lower tree coverage in areas dominated by tallgrass prairie should not be penalized."

               )
        )
        )
    })
    

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
    
    
    output$priority_para <- renderUI({
      ns <- session$ns
      req(geo_selections$selected_area)
      tagList(
        if (map_selections$priority_layer == "Off") {HTML(paste0("No prioritization layer was used."))
          } else {
            
          ps <- filter(map_util$map_data2,
                       tract_string %in% c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == geo_selections$selected_area, ]$tract_id))
                
                
        para <- HTML(paste0( 
          "Understanding the intersection of the tree canopy, people, and the built environment is important for prioritization and planning efforts. Based on the ",
          tolower(map_selections$preset), 
          " preset used, tracts within ",
          geo_selections$selected_area,
          " have priority scores ranging from ",
          round(min(ps$MEAN), 2), " to ", round(max(ps$MEAN), 2),
          " (out of 10, with 10 indicating the highest priority). ",
          " The variables included in this ranking are: <br><br> - ",
          
          if(map_selections$preset == "Environmental justice") {
            paste(unlist(((metadata[metadata$ej == 1, ]$name))), collapse = ",<br>- ")
          } else if(map_selections$preset == "Climate change") {
            paste(unlist(((metadata[metadata$cc == 1, ]$name))), collapse = ",<br>- ")
          } else if(map_selections$preset == "Public health") {
            paste(unlist(((metadata[metadata$ph == 1, ]$name))), collapse = ",<br>- ")
          } else if(map_selections$preset == "Conservation") {
            paste(unlist(((metadata[metadata$cons == 1, ]$name))), collapse = ",<br>- ")
          } else {map_selections$allInputs}, 

          "<br><br> The plot below shows overall priority score as well as the individual score for each of the selected variables for each tract within ",
          geo_selections$selected_area,
          ". Because the scores are standardized and scaled from 0-10, the regional average score is approximately 5. A full data table with the raw values can be downloaded at the end of this report. Please see the Methods for more detail.<br>"
        )
        )
        return(para)
        }
      )
    })
    
    
    output$priority_plot <- renderPlot({
      req(geo_selections$selected_area)
      
      # eva_data_main %>%
      #   filter(name %in% test) %>%
      #   # filter(name %in% map_selections$allInputs) %>%
      #   ungroup() %>%
      #   select(tract_string, name, weights_scaled, raw_value) %>%
      #   mutate(flag = if_else (tract_string %in% c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == "Lake Elmo", ]$tract_id), "selected", NA_character_)) %>%
      #   mutate(across(c(raw_value), ~ifelse(str_detect(name, c("%")), . * 100, .))) %>%
      #   mutate(across(where(is.numeric), round, 2)) %>%
      #   # arrange(tract_string, name) %>%
      #   rename(`Tract id` = tract_string,
      #          Variable = name,
      #          `Standardized value` = weights_scaled,
      #          `Raw value` = raw_value) %>%
      #   pivot_wider(names_from = Variable, values_from = c( `Raw value`, `Standardized value`),
      #               names_sep = ("; "))

      ps <- filter(map_util$map_data2,
                   tract_string %in% c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == geo_selections$selected_area, ]$tract_id)) %>%
        st_drop_geometry() %>%
        rename(weights_scaled = MEAN) %>%
        add_column(name = "Aggregated priority score") %>%
        add_column(order = "second")
      
      plot <- eva_data_main %>%
        # filter(name %in% test) %>%
          filter(name %in% 
                   if(map_selections$preset == "Environmental justice") {
                     metadata[metadata$ej == 1, ]$name
                     } else if(map_selections$preset == "Climate change") {
                       metadata[metadata$cc == 1, ]$name
                     } else if(map_selections$preset == "Public health") {
                       metadata[metadata$ph == 1, ]$name
                     } else if(map_selections$preset == "Conservation") {
                       metadata[metadata$cons == 1, ]$name
                     } else {map_selections$allInputs}) %>%
        mutate(flag = if_else (tract_string %in% c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == geo_selections$selected_area, ]$tract_id), "selected", NA_character_)) %>%
        filter(flag == "selected") %>%
        add_column(order = "first") %>%
        filter(!is.na(weights_scaled)) %>%
        bind_rows(ps) %>%
        ggplot(aes(y = weights_scaled, x = fct_reorder(name, order), col = tract_string, group = tract_string))+
        geom_point(col = "black",
                   position = position_dodge(width = .2)) + geom_line(col = "black", alpha = .2,
                                                                      position = position_dodge(width = .2)) +
        councilR::council_theme() +
        ylim(c(0,10)) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 40))+
        theme(panel.grid.major.x = element_blank(),
              axis.title.y = element_blank()) +
        labs(y = "Score (out of 10)") +
        coord_flip() 
      
      return(plot)
    })
    
    output$equity_para <- renderUI({
      ns <- session$ns
      req(geo_selections$selected_area)
      tagList(
        if (map_selections$priority_layer == "Off") {HTML(paste0("No prioritization layer was used."))
        } else {
          
          ps <- filter(map_util$map_data2,
                       tract_string %in% c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == geo_selections$selected_area, ]$tract_id))
          
          
          para <- HTML(paste0( 
            "Research shows that trees are unevenly distributed across communities. In particular, neighborhoods with high BIPOC or low-income populations have less tree canopy (MacDonald 2021) than areas which were historically redlined (NPR news story, Locke et al. 2021, Namin et al. 2020). Addressing inequity in tree canopy cover may reduce heat-related deaths by up to 25% (Sinha 2021).<br><br>",
            "At the MetCouncil, we have shown that areas where the annual median income is <$100,000 and areas with high BIPOC populations have less tree canopy and greenness. We are specifically calling out these variables in figures below. Tracts within ", 
            geo_selections$selected_area,
            " are in green, and the regional trend is in blue."
          )
          )
          return(para)
        }
      )
    })
    
    output$equity_plot <- renderPlot({
      req(geo_selections$selected_area)
      
      equityplot <- eva_data_main %>% 
        filter(variable %in% c("pbipoc", "canopy_percent", "ndvi", "mdhhincnow")) %>%
        select(tract_string, variable, raw_value) %>%
        pivot_wider(names_from = variable, values_from = raw_value) %>%
        # mutate(flag = if_else(tract_string %in% c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == "Minneapolis", ]$tract_id), "selected", NA_character_))
        mutate(flag = if_else(tract_string %in% 
                                if (geo_selections$selected_geo == "ctus") {
                                  c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == geo_selections$selected_area, ]$tract_id)
                                } else {
                                  c(nhood_crosswalk[nhood_crosswalk$GEO_NAME == geo_selections$selected_area, ]$tract_id)
                                }, "selected", NA_character_))
      
        race_equity <- equityplot %>%
          ggplot(aes(x = pbipoc, y = canopy_percent)) + 
          geom_point(col = "grey40", alpha = .3, data = filter(equityplot, is.na(flag))) + 
          geom_smooth(method = "lm", fill = NA, col = councilR::colors$councilBlue, data = equityplot) +
          geom_point(fill = councilR::colors$cdGreen, size = 5, col = "black", pch = 21, data = filter(equityplot, flag == "selected")) + 
          councilR::council_theme() + 
          scale_x_continuous(labels = scales::percent_format(accuracy = 1)) + 
          scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
          labs(x = "BIPOC population\n(%)", y = "Tree canopy coverage in tract\n(%)")
        
        
        inc_equity <- equityplot%>%
          ggplot(aes(x = mdhhincnow, y = (canopy_percent))) + 
          geom_point(col = "grey40", alpha = .3, data = filter(equityplot, is.na(flag))) + 
          geom_smooth(method = "lm", fill = NA, col = councilR::colors$councilBlue) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
          scale_x_continuous(labels = scales::dollar_format(accuracy = 1)) + 
          geom_point(fill = councilR::colors$cdGreen, size = 5, col = "black", pch = 21, data = filter(equityplot, flag == "selected")) + 
          councilR::council_theme() + 
          labs(x = "Median household income", y = "")
        
        fig_equity <- cowplot::plot_grid(race_equity, inc_equity, labels = "AUTO")
      
      return(fig_equity)
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
