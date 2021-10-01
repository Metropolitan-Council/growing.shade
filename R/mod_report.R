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

            fluidRow(column(width = 6, 
                        uiOutput(ns("get_the_report"))),
                 column(width = 6,
                        uiOutput(ns("get_the_data")))),
            
            h3(uiOutput(ns("geoarea"))),
            
      h4("Tree canopy summary"),
      uiOutput(ns("tree_para")),
      plotOutput(ns("tree_plot"), "200px", width = "100%") %>%
        shinyhelper::helper(type = "markdown", content = "LineplotHelp", size = "m"),
      
      h4("Priortization summary"),
      uiOutput(ns("rank_para")),
      plotOutput(ns("rank_plot"), "100px", width = "100%"), 
      br(),
      uiOutput(ns("priority_para")),
      plotOutput(ns("priority_plot"), "400px", width = "100%"), 

      h4("Equity analysis"),
      uiOutput(ns("equity_para")),
      plotOutput(ns("equity_plot"), "400px", width = "80%"), # %>%
      
      h4("Other considerations"),
      uiOutput(ns("other_para")),
      plotOutput(ns("other_plot"), "300px", width = "80%"), 

            h4("Other resources"),
      uiOutput(ns("resource_para")),
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
 
    ####### things to export
    param_area <- reactive({
      req(geo_selections$selected_area)
      output <- geo_selections$selected_area
      return(output)
    })
    
    param_min <- reactive({
      req(geo_selections$selected_area)
      output <- if (geo_selections$selected_geo == "ctus") {
        ctu_list[ctu_list$GEO_NAME == param_area(), ]$min} else {
          nhood_list[nhood_list$GEO_NAME == param_area(), ]$min
        }
      return(output)
    })
    
    param_max <- reactive({
      req(geo_selections$selected_area)
      output <- if (geo_selections$selected_geo == "ctus") {
        ctu_list[ctu_list$GEO_NAME == param_area(), ]$max} else {
          nhood_list[nhood_list$GEO_NAME == param_area(), ]$max
        }
      return(output)
    })
    
    param_ntracts <- reactive({
      req(geo_selections$selected_area)
      output <- if (geo_selections$selected_geo == "ctus") {
        ctu_list[ctu_list$GEO_NAME == param_area(), ]$ntracts} else {
          nhood_list[nhood_list$GEO_NAME == param_area(), ]$ntracts
        }
      return(output)
    })
    
    
    
    
    
    
    #### things to populate report
    output$geoarea <- renderUI({
      ns <- session$ns
      tagList(
        paste0("Custom report for ", param_area())
      )
    })    

    output$tree_para <- renderUI({
      ns <- session$ns
      req(geo_selections$selected_area)
      tagList(HTML(
        paste0(
          param_area(),
          " has an existing tree canopy which ranges from ",
          param_min(),
          "% to ",
          param_max(),
          "% ",
          "across ",
          param_ntracts(),
          " Census tracts. The distribution of tree canopy across the region is shown below; tracts in ",
          param_area(),
          " are highlighted in green.<br><br>",
          " In most areas in our region, a tree canopy coverage of 40% (as detected by our methods) leads to the greatest benefits. Note that native tallgrass prairie occurs throughout our region - lower tree coverage in areas dominated by tallgrass prairie should not be penalized."
        )
      ))
    })
    

    output$tree_plot <- renderPlot({
      req(geo_selections$selected_area)

      canopyplot<- eva_data_main %>%
        filter(variable %in% c("canopy_percent")) %>%
        select(tract_string, variable, raw_value) %>%
        mutate(flag = if_else(tract_string %in% 
                                if (geo_selections$selected_geo == "ctus") {
                                  c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                                  } else {
                                    c(nhood_crosswalk[nhood_crosswalk$GEO_NAME == param_area(), ]$tract_id)
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
    
    
    output$rank_para <- renderUI({
      ns <- session$ns
      req(geo_selections$selected_area)
      tagList(
        if (map_selections$priority_layer == "Off") {HTML(paste0("No prioritization layer was used. To change this, please scroll back up to the top and turn 'on' the priority layer."))
          } else {
            
          ps <- filter(map_util$map_data2,
                       tract_string %in% 
                         if (geo_selections$selected_geo == "ctus") {
                           c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                         } else {
                           c(nhood_crosswalk[nhood_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                         })
                
                
        para <- HTML(paste0( 
          "Understanding the intersection of the tree canopy, people, and the built environment is important for prioritization and planning efforts. Based on the ",
          tolower(map_selections$preset), 
          " preset used, tracts within ",
          param_area(),
          " have priority scores ranging from ",
          round(min(ps$MEAN), 2), " to ", round(max(ps$MEAN), 2),
          " (out of 10, with 10 indicating the highest priority). The ranking of these overall priority scores are shown below. A rank of 1 indicates the tract with the highest priority (out of the 704 tracts across the region).<br><br>"
        )
        )
        return(para)
        }
      )
    })
    
    output$rank_plot <- renderPlot({
      req(geo_selections$selected_area)
    test <- filter(map_util$map_data2,
                   tract_string %in% 
                     if (geo_selections$selected_geo == "ctus") {
                       c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                     } else {
                       c(nhood_crosswalk[nhood_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                     }) %>%
      st_drop_geometry() 
    
    plot <- if (map_selections$priority_layer == "Off") {print("nothing to see here")
      } else {ggplot() +
      scale_x_continuous( limits = c(1, 704), labels = c(1, 250, 500, 704), breaks = c(1, 250, 500, 704)) +
      ylim(0, 1) +
      geom_vline(data = test,
                 aes(xintercept = RANK)) +
      councilR::council_theme() +
      theme(axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      geom_segment(aes(x = 1, xend = 700, y = 0, yend = 0))+
      labs(x = "Rank of aggregated priority score\n(out of 704 tracts across the region)")
      }
    return(plot)
    })
    
    output$priority_para <- renderUI({
      ns <- session$ns
      req(geo_selections$selected_area)
      tagList(
        if (map_selections$priority_layer == "Off") {HTML(paste0(""))
        } else {

          para <- HTML(paste0( 
            "The variables included in this prioritization layer are: <br> - ",
            
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
            param_area(),
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
                   tract_string %in% 
                     if (geo_selections$selected_geo == "ctus") {
                       c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                       } else {
                         c(nhood_crosswalk[nhood_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                       }) %>%
        st_drop_geometry() %>%
        rename(weights_scaled = MEAN) %>%
        add_column(name = "Aggregated priority score") %>%
        add_column(order = "first")
      
      plot <- if (map_selections$priority_layer == "Off") {print("nothing to see here")
      } else {eva_data_main %>%
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
        mutate(flag = if_else(tract_string %in% 
                                if (geo_selections$selected_geo == "ctus") {
                                  c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                                } else {
                                  c(nhood_crosswalk[nhood_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                                }, "selected", NA_character_)) %>%
        filter(flag == "selected") %>%
        add_column(order = "second") %>%
        filter(!is.na(weights_scaled)) %>%
        bind_rows(ps) %>%
        ggplot(aes(y = weights_scaled, x = fct_reorder(name, order, .desc = TRUE), col = tract_string, group = tract_string))+
        geom_point(col = "black",
                   position = position_dodge(width = .2)) + 
          geom_line(col = "black", alpha = .2,
                    position = position_dodge(width = .2)) +
          # geom_line(col = "blue",
          #           data = metadata %>% 
          #             filter(name %in%
          #                      if(map_selections$preset == "Environmental justice") {
          #                        metadata[metadata$ej == 1, ]$name
          #                      } else if(map_selections$preset == "Climate change") {
          #                        metadata[metadata$cc == 1, ]$name
          #                      } else if(map_selections$preset == "Public health") {
          #                        metadata[metadata$ph == 1, ]$name
          #                      } else if(map_selections$preset == "Conservation") {
          #                        metadata[metadata$cons == 1, ]$name
          #                      } else {map_selections$allInputs}) %>%
          #             # filter(name %in% metadata[metadata$ej == 1, ]$name) %>%
          #             full_join(tibble(name = "Aggregated priority score"),
          #                       MEANSCALED = NA, by = 'name') %>%
          #             add_column(tract_string = "rgn"),
          #           aes(y = MEANSCALED)) +
        councilR::council_theme() +
        ylim(c(0,10)) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 40))+
        theme(panel.grid.major.x = element_blank(),
              axis.title.y = element_blank()) +
        labs(y = "Score (out of 10,\nwhere 10 indicates higest priority)") +
        coord_flip() 
      }
      
      return(plot)
    })
    
    output$equity_para <- renderUI({
      ns <- session$ns
      req(geo_selections$selected_area)
          para <- HTML(paste0( 
            "Research shows that trees are unevenly distributed across communities. In particular, neighborhoods with high BIPOC or low-income populations have less tree canopy (",
            a("MacDonald 2021",
              href = "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0249715",
              .noWS = "outside",
              target = "_blank"),
              ") than areas which were historically redlined (", 
            a("NPR news story, ",
              href = "https://www.npr.org/2020/01/14/795961381/racist-housing-practices-from-the-1930s-linked-to-hotter-neighborhoods-today",
              .noWS = "outside",
              target = "_blank"), 
            a("Locke et al. 2021, ",
              href = "https://www.nature.com/articles/s42949-021-00022-0",
              .noWS = "outside",
              target = "_blank"), 
            a("Namin et al. 2020",
              href = "https://www.sciencedirect.com/science/article/abs/pii/S0277953619307531?via%3Dihub",
              .noWS = "outside",
              target = "_blank"), 
            "). Addressing inequity in tree canopy cover may reduce heat-related deaths by up to 25% (",
            a("Sinha 2021",
              href = "https://www.fs.fed.us/nrs/pubs/jrnl/2021/nrs_2021_paramita_001.pdf",
              .noWS = "outside",
              target = "_blank"), 
              ").<br><br>",
            "At the MetCouncil, we have shown that areas where the annual median income is <$100,000 and areas with high BIPOC populations have less tree canopy and greenness. We are specifically calling out these variables in figures below. Tracts within ", 
            param_area(),
            " are in green, and the regional trend is in blue."
          )
          )
          return(para)
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
                                  c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                                } else {
                                  c(nhood_crosswalk[nhood_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                                }, "selected", NA_character_))
      
        race_equity <- equityplot %>%
          ggplot(aes(x = pbipoc, y = canopy_percent)) + 
          geom_point(col = "grey40", alpha = .3, data = filter(equityplot, is.na(flag))) + 
          geom_smooth(method = "lm", fill = NA, col = councilR::colors$councilBlue, data = equityplot) +
          geom_point(fill = councilR::colors$cdGreen, size = 5, col = "black", pch = 21, data = filter(equityplot, flag == "selected")) + 
          councilR::council_theme() + 
          scale_x_continuous(labels = scales::percent_format(accuracy = 1)) + 
          scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
          labs(x = "BIPOC population\n(%)", y = "Tree canopy\ncoverage (%)")
        
        
        inc_equity <- equityplot%>%
          ggplot(aes(x = mdhhincnow, y = (canopy_percent))) + 
          geom_point(col = "grey40", alpha = .3, data = filter(equityplot, is.na(flag))) + 
          geom_smooth(method = "lm", fill = NA, col = councilR::colors$councilBlue) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
          scale_x_continuous(labels = scales::dollar_format(accuracy = 1)) + 
          geom_point(fill = councilR::colors$cdGreen, size = 5, col = "black", pch = 21, data = filter(equityplot, flag == "selected")) + 
          councilR::council_theme() + 
          labs(x = "Median household income", y = "Tree canopy\ncoverage (%)")
        
        fig_equity <- cowplot::plot_grid(race_equity, inc_equity, nrow = 2, labels = "AUTO")
      
      return(fig_equity)
    })
    
    
    
    output$other_para <- renderUI({
      ns <- session$ns
      req(geo_selections$selected_area)
      tagList(
          HTML(paste0( 
            "The goal of this section is present information about biodiversity, management challenges, and other considerations for managing the tree canopy.<br><br>",
            "Invasion by the Emerald ash borer (EAB) insect is a major threat to existing tree canopy. Data shows that EAB has infested ", 
            eab %>% sf::st_intersection(filter(if (geo_selections$selected_geo == "ctus") {
              ctu_list
            } else {nhood_list}, GEO_NAME == param_area())) %>% nrow(), " trees in ",
            param_area(), " (",
            a("Minnesota DNR",
              href = "https://mnag.maps.arcgis.com/apps/webappviewer/index.html?id=63ebb977e2924d27b9ef0787ecedf6e9",
              .noWS = "outside",
              target = "_blank"), 
            "). Please note that these data are not necessarily intended to identify every ash tree (infested or not), however this information may still be useful.<br><br>",
            "Low biodiversity is another threat to the tree canopy in the region. And knowing which species can adapt to a changing climate. Over the last 100 years, our region has seen a decline in oak trees, and an increase in ash, elm, and maple trees (<a href = 'https://gisdata.mn.gov/dataset/biota-original-pls-bearing-trees' target = '_blank'>Almendinger 1997</a>, <a href = 'https://www.nrs.fs.fed.us/data/urban/state/city/?city=6#ufore_data' target = '_blank'>Davey Resource Group 2004</a>). 'Other' species make up a larger percent of the tree canopy today, but these species are mostly introduced species rather than a diverse assemblage of native species (as was the case before 1900). "
          )
          )
      )
    })
    
    
    output$other_plot <- renderPlot({

      treebiodiv %>%
        ggplot(aes(x = (timepoint), y = percent, fill = spp_name, shape = spp_name)) +
        geom_line( position = position_dodge(width = 10))+#, aes(col = spp_name)) +
        geom_point(
          size = 5, position = position_dodge(width = 10)) + 
        scale_fill_brewer(palette = "Paired", name = "Species") +
        scale_color_brewer(palette = "Paired", name = "Species") +
        scale_shape_manual(values = rep(c(21:25), 3), name = "Species")+
        councilR::council_theme() +
        labs(x = "Year", y = "Species composition (%)") +
        guides(fill = guide_legend(nrow = 6, byrow = T),
               color = guide_legend(nrow = 6, byrow = T),
               shape = guide_legend(nrow = 6, byrow = T))
      
    })
    
    
    output$resource_para <- renderUI({
      ns <- session$ns
      tagList(
        HTML(paste0( 
          "Managing the tree canopy is a complex and important subject. Growing Shade is a unique tool offering users the ability to customize prioritization and see detailed maps of tree canopy gaps. It is under active development, so please check back or contact us for more details. However there are other tools which may be useful, and it should be noted that there are still many unanswered questions. The list below has been compiled as a starting point.<br><br>", 
          "Finally, our experience and research tells us that data cannot substitute for engagement with prioritized stakeholders to understand community-specific concerns or opportunities. The on-the-ground knowledge of residents and the expertise of practitioners are valuable sources of information necessary to enhance and refine the shared understanding of this data.<br><br>",
          "<strong>Tools</strong> - What additional tools and assessments exist to help prioritize where to plant trees and maintain tree canopy? <br>-	",
          a("American Forests Tree Equity Score project",
            href = "https://www.americanforests.org/our-work/tree-equity-score/",
            .noWS = "outside",
            target = "_blank"),
          "<br>- ", 
          a("Hennepin County, MN Tree Canopy Tree Planting Priority Areas",
            href = "https://gis-hennepin.opendata.arcgis.com/pages/tree-planting",
            .noWS = "outside",
            target = "_blank"),
          "<br>- ",
          a("City of Saint Paul Urban Tree Canopy Assessment 2011",
            href = "https://www.stpaul.gov/departments/parks-recreation/natural-resources/forestry/urban-tree-canopy-assessment",
            .noWS = "outside",
            target = "_blank"),
          "<br><br><strong>Information, Guides and Toolkits</strong> - Where can I lean more about the benefits provided by urban forests and learn how to build them?  <br>-	",
          a("Vibrant Cities Lab",
            href = "https://www.vibrantcitieslab.com/",
            .noWS = "outside",
            target = "_blank"),
          "<br><br><strong>Climate Change</strong> - Where can I find more information about climate change impacts in the Twin Cities? <br>- ",
          a("Climate Vulnerability Assessment by Metropolitan Council for the Twin Cities",
            href = "https://www.fs.usda.gov/sites/default/files/fs_media/fs_document/urbannatureforhumanhealthandwellbeing_508_01_30_18.pdf%22 %EF%BF%BDHYPERLINK %22https://metrocouncil.org/Communities/Planning/Local-Planning-Assistance/CVA.aspx",
            .noWS = "outside",
            target = "_blank"),
          "<br>- ",
          a("Extreme Heat Map Tool",
            href = "https://metrocouncil.maps.arcgis.com/apps/webappviewer/index.html?id=fd0956de60c547ea9dea736f35b3b57e",
            .noWS = "outside",
            target = "_blank"), 
          " by Metropolitian Council<br>- ",
          a("Extreme Heat Story Map",
            href = "https://metrocouncil.maps.arcgis.com/apps/MapJournal/index.html?appid=7d9cdd3929e9439bb5b25aa1186d5783",
            .noWS = "outside",
            target = "_blank"),
          " by Metropolitian Council",
          
          "<br><br><strong>Human Health</strong> - Where can I learn about the impacts of tree on human health? <br>- ",
          a("US Forest Service Report: Urban Nature for Human Health and Well-being 2018",
            href = "https://www.fs.usda.gov/sites/default/files/fs_media/fs_document/urbannatureforhumanhealthandwellbeing_508_01_30_18.pdf",
            .noWS = "outside",
            target = "_blank"),
          
          "<br><br><strong>What’s Next</strong> - What new projects are underway that could support and inform urban forests in the Twin Cities?  <br>- ",
          a("Urban LTER (Long-term Ecological Research) in the Twin Cities",
            href = "https://mspurbanlter.umn.edu/overview",
            .noWS = "outside",
            target = "_blank")
        )
        )
      )
    })
    
    

    output$dl_report <- downloadHandler(
      filename = paste0("GrowingShade_", Sys.Date(), ".html"),
      content = function(file) {
        tempReport <- file.path(tempdir(), "report_new.Rmd")
        file.copy("report_new.Rmd", tempReport, overwrite = TRUE)
        # Set up parameters to pass to Rmd document
        params <- list(param_area = param_area(),
                       param_min = param_min(),
                       param_max = param_max(),
                       param_ntracts = param_ntracts()
                       
                       
                       # selected_geo = input$geo,
                       # selected_city = input$cityInput,
                       # vars_used = map_selections$preset,
                       # priority_score = map_util$map_data2,#round(map_util$map_data2$MEAN, 3),
                       # rank_total = nrow(map_util$map_data2),
                       # vars_selected = map_selections$allInputs,
                       # canopy = map_util$canopycov
                       )
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
    
    output$get_the_report <- renderUI({
      req(geo_selections$selected_area)
      downloadButton(ns('dl_report'), label = 'Download this report') })
    
    
    output$dl_data <- downloadHandler(
      filename = function() {paste0("GrowingShade_MetCouncil_", param_area(), "_", Sys.Date(), ".xlsx")},
      content = function(file) {writexl::write_xlsx(
        list("Metadata" = metadata %>%
               rbind(c(""), c("Please use caution if using Excel formatting. You may need to divide cells by 100 for Excel to recognize percents correctly.", "", ""), 
                     c("This data is obviously not finished. If you are seeing this warning, please do not use!.", "", ""), 
                     c("The interactive tool can be accessed at <https://metrotransitmn.shinyapps.io/growing-shade/>.", "", "")),
             "Test" = (c("yeah, nothing yet!")) # "Counties" = (eva_data_main)
             ),
        path = file)}
    )
    
    output$get_the_data <- renderUI({
      req(geo_selections$selected_area)
      downloadButton(ns('dl_data'), label = 'Download data only') })
        
        
  })
}
    
## To be copied in the UI
# mod_report_ui("report_ui_1")
    
## To be copied in the server
# mod_report_server("report_ui_1")
