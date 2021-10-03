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
    
    # uiOutput(ns("instructions")),
    
    fluidRow(column(width = 6, uiOutput(ns("get_the_report"))),
             column(width = 6,uiOutput(ns("get_the_data")))),
    (uiOutput(ns("geoarea"))),
            
    uiOutput(ns("tree_title")),
    uiOutput(ns("tree_para")),
      uiOutput(ns("get_tree_plot")),

    uiOutput(ns("priority_title")),
    uiOutput(ns("rank_para")),
      uiOutput(ns("get_rank_plot")),
    br(),
      uiOutput(ns("priority_para")),
      uiOutput(ns("get_priority_plot")),
    # br(),
      # uiOutput(ns("table_para")),
     # dataTableOutput(ns("priority_table")),
    # tableOutput(ns("priority_table")),

    uiOutput(ns("equity_title")),
    uiOutput(ns("equity_para")),
      uiOutput(ns("get_equity_plot")),

    uiOutput(ns("other_title")),
    uiOutput(ns("other_para"))#,
      # uiOutput(ns("get_other_plot"))#,

    # uiOutput(ns("resource_title")),
    # uiOutput(ns("resource_para")),
    )
    
}
    
#' report Server Functions
#'
#' @noRd 
mod_report_server <- function(id,
                              geo_selections,
                              map_selections,
                              tract_selections,
                              map_util){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    ####### things to export
    TEST <- reactive({
      TEST <- if (geo_selections$selected_geo == "ctus") {
        geo_selections$selected_area
      } else if (geo_selections$selected_geo == "nhood") {
        geo_selections$selected_area
        } else if (geo_selections$selected_geo == "tracts") {
          tract_selections$selected_tract}
      return(TEST)
    })
    
    param_area <- reactive({
      req(TEST() != "")
      output <- TEST()
      return(output)
    })
    
    param_min <- reactive({
      req(TEST() != "")
      output <- if (geo_selections$selected_geo == "ctus") {
        ctu_list[ctu_list$GEO_NAME == param_area(), ]$min
        } else if (geo_selections$selected_geo == "nhood") {
          nhood_list[nhood_list$GEO_NAME == param_area(), ]$min
        } else {NA}
      return(output)
    })
    
    param_max <- reactive({
      req(TEST() != "")
      output <- if (geo_selections$selected_geo == "ctus") {
        ctu_list[ctu_list$GEO_NAME == param_area(), ]$max
      } else if (geo_selections$selected_geo == "nhood") {
        nhood_list[nhood_list$GEO_NAME == param_area(), ]$max
        } else {round(eva_data_main[eva_data_main$tract_string == param_area() & eva_data_main$variable == "canopy_percent", ]$raw_value * 100, 1)}
      return(output)
    })
    
    param_ntracts <- reactive({
      req(TEST() != "")
      output <- if (geo_selections$selected_geo == "ctus") {
        ctu_list[ctu_list$GEO_NAME == param_area(), ]$ntracts
      } else if (geo_selections$selected_geo == "nhood") {
          nhood_list[nhood_list$GEO_NAME == param_area(), ]$ntracts
        } else {NA}
      return(output)
    })

    param_selectedtractvalues <- reactive({
      req(TEST() != "")
      output <- filter(map_util$map_data2,
                       tract_string %in% 
                         if (geo_selections$selected_geo == "ctus") {
                           c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                         } else if (geo_selections$selected_geo == "nhood") {
                           c(nhood_crosswalk[nhood_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                         } else {
                           c(param_area())
                         })
      return(output)
    })
    
    
    param_dl_data <- reactive({
      req(geo_selections$selected_area)
      ps <- param_selectedtractvalues() %>%
        st_drop_geometry() %>%
        rename(raw_value = MEAN) %>%
        add_column(name = "Aggregated priority score") %>%
        add_column(order = 1)
      
      output <- eva_data_main %>%
        filter(name %in%
                 if(map_selections$preset == "Environmental justice") {
                   metadata[metadata$ej == 1, ]$name
                 } else if(map_selections$preset == "Climate change") {
                   metadata[metadata$cc == 1, ]$name
                 } else if(map_selections$preset == "Public health") {
                   metadata[metadata$ph == 1, ]$name
                 } else if(map_selections$preset == "Conservation") {
                   metadata[metadata$cons == 1, ]$name
                 } else if(map_selections$preset == "Custom") {
                   c(map_selections$allInputs$value)}) %>%
        mutate(flag = if_else(tract_string %in%
                                if (geo_selections$selected_geo == "ctus") {
                                  c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                                } else {
                                  c(nhood_crosswalk[nhood_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                                }, "selected", NA_character_)) %>%
        filter(flag == "selected") %>%
        add_column(order = 2) %>%
        # filter(!is.na(weights_scaled)) %>%
        bind_rows(ps) %>%
        ungroup() %>%
        select(tract_string, name, raw_value) %>%
        mutate(flag = if_else (tract_string %in% c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == "Lake Elmo", ]$tract_id), "selected", NA_character_)) %>%
        mutate(across(c(raw_value), ~ifelse(str_detect(name, c("%")), . * 100, .))) %>%
        mutate(across(where(is.numeric), round, 2)) %>%
        # arrange(tract_string, name) %>%
        rename(`Tract id` = tract_string,
               Variable = name,
               `Raw value` = raw_value) %>%
        pivot_wider(names_from = Variable, values_from = `Raw value`) %>%
        ungroup() %>%
        select(-flag)
      
      return(output)
    })
    
    
    param_test <- reactive({
      req(geo_selections$selected_area)
      
    })
    
    
    #### things to populate report
    # output$instructions <- renderUI({
    #   ns <- session$ns
    #   tagList(
    #     if (TEST() == "") {
    #       "A custom analysis will be generated for you here. Please select either a specific city or neighborhood from the dropdown menu above in order to see the analysis. The resulting report and raw data will also be available here for download."} else {""}
    #   )
    # })    
    
    
    output$geoarea <- renderUI({
      ns <- session$ns
      tagList(
        HTML(paste0("<h3>Summary report for ", param_area(), "</h3>"))
      )
    })    

    # tree canopy section ------------
    output$tree_para <- renderUI({
      ns <- session$ns
      req(TEST() != "")
      tagList(HTML(
        paste0(
          if(geo_selections$selected_geo == "tracts") {
            paste0("Tract ", param_area(), " has an existing tree canopy of ", param_max(), 
            "%.  The distribution of tree canopy across the region is shown below; the selected tract is highlighted in green. <br><br>")
            } else { 
          paste0(param_area(),
          " has an existing tree canopy which ranges from ",
          param_min(),
          "% to ",
          param_max(),
          "% ",
          "across ",
          param_ntracts(),
          " Census tracts. The distribution of tree canopy across the region is shown below; tracts in ",
          param_area(),
          " are highlighted in green.")}#,
          # "<br><br> In most areas in our region, a tree canopy coverage of 40% (as detected by our methods) leads to the greatest benefits. Lower tree coverage in areas with native tallgrass prairie should not be penalized."
        )
      ))
    })
    

    output$tree_plot <- renderPlot({
      req(TEST() != "")
      
      canopyplot<- eva_data_main %>%
        filter(variable %in% c("canopy_percent")) %>%
        select(tract_string, variable, raw_value) %>%
        mutate(flag = if_else(tract_string %in% 
                                if (geo_selections$selected_geo == "ctus") {
                                  c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                                  } else if (geo_selections$selected_geo == "nhood") {
                                    c(nhood_crosswalk[nhood_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                                  } else {c(param_area())}, "selected", NA_character_))
      plot <- ggplot()+
        ggdist::stat_halfeye(
          data = canopyplot, aes(x = raw_value, y = 1),
          adjust = .5,  width = .6,  .width = 0,  justification = -.6, 
          point_colour = NA,
          na.rm = T) + 
        geom_boxplot(data = canopyplot, aes(x = raw_value, y = 1),
                     width = .75, outlier.shape = NA,
                     na.rm = T) +
        councilR::council_theme() +
        theme(panel.grid.minor = element_blank(),
              panel.grid.major.y = element_blank(),
              axis.text.y = element_blank()) +
        geom_point(size = 1.3,alpha = .3,
                   position = position_jitter(seed = 1, width = 0, height = .3),
                   col = "grey40",
                   aes(x = raw_value, y = 1),
                   data = filter(canopyplot, is.na(flag)),
                   na.rm = T) +
        labs(y = "", x = "Tree canopy cover (%)") +
        scale_x_continuous(labels = scales::percent_format(accuracy = 1)) + 
        geom_jitter(aes(x = raw_value, y = 1), 
                    position = position_jitter(seed = 1, width = 0, height = .3), 
                   fill = councilR::colors$cdGreen, 
                   size = 5, col = "black", pch = 21, 
                   data = filter(canopyplot, flag == "selected"),
                   na.rm = T)
      return(plot)
    })
    
    # ranking section ------------
    
    output$rank_para <- renderUI({
      ns <- session$ns
      req(TEST() != "")
      # tagList(
        # if (map_selections$priority_layer == "Off") {HTML(paste0("No prioritization layer was used. To change this, please scroll back up to the top and turn 'on' the priority layer."))
        #   } else {

        para <- HTML(paste0( 
          # "Understanding the intersection of the tree canopy, people, and the built environment is important for prioritization and planning efforts. ",
          "Using the ",
          tolower(map_selections$preset), 
          " preset, ", 
          if (geo_selections$selected_geo == "tracts") {
            paste0(" tract ", param_area(), " has a priority score of ", 
                   round((param_selectedtractvalues()$MEAN), 2), 
                   " with a region-wide ranking of ", 
                   (param_selectedtractvalues()$RANK), ". A plot of the tract rankings are shown below.<br><br>")
          } else {paste0("tracts within ",
          param_area(),
          " have overall priority scores ranging from ",
          round(min(param_selectedtractvalues()$MEAN), 2), " to ", round(max(param_selectedtractvalues()$MEAN), 2),
          " and a region-wide ranking from ",
          min(param_selectedtractvalues()$RANK), " to ", max(param_selectedtractvalues()$RANK), " (where a higher rank (closer to 1) indicates higher priorities). A plot of the tract rankings are shown below.<br><br>")}
        )
        )
        return(para)
        # }
      # )
    })
    
    output$rank_plot <- renderPlot({
      req(TEST() != "")
      
      test <- param_selectedtractvalues() %>%
      st_drop_geometry() 
    
    plot <- #if (map_selections$priority_layer == "Off") {print("nothing to see here")
      # } else {
        ggplot() +
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
      # }
    return(plot)
    })
    
    # priority section -----------
    
    output$priority_para <- renderUI({
      ns <- session$ns
      req(TEST() != "")
      # tagList(
      #   if (map_selections$priority_layer == "Off") {HTML(paste0(""))
      #   } else {

          para <- HTML(paste0( 
            # "The variables included in this prioritization layer are: <br> - ",
            # 
            # if(map_selections$preset == "Environmental justice") {
            #   paste(unlist(((metadata[metadata$ej == 1, ]$name))), collapse = ",<br>- ")
            # } else if(map_selections$preset == "Climate change") {
            #   paste(unlist(((metadata[metadata$cc == 1, ]$name))), collapse = ",<br>- ")
            # } else if(map_selections$preset == "Public health") {
            #   paste(unlist(((metadata[metadata$ph == 1, ]$name))), collapse = ",<br>- ")
            # } else if(map_selections$preset == "Conservation") {
            #   paste(unlist(((metadata[metadata$cons == 1, ]$name))), collapse = ",<br>- ")
            # } else if(map_selections$preset == "Custom") {
            #   paste(unlist(((map_selections$allInputs))), collapse = ",<br>- ") #this is the right format for here
            #   }, 
            # 
            # "<br><br> ",
            "This is how the selected area compares to the region for the proritization variables used. Scaled and standardized scores are on the x-axis. The selected area is shown in green and the regional average in blue.<br>"
          )
          )
          return(para)
      #   }
      # )
    })
    
    
#old priority plot with lineplot ------
    # output$priority_plot <- renderPlot({
    #   req(TEST() != "")
    #   
    #   ps <- param_selectedtractvalues() %>%
    #     st_drop_geometry() %>%
    #     rename(weights_scaled = MEAN) %>%
    #     add_column(name = "Aggregated priority score") %>%
    #     add_column(order = 1)
    #   
    #   plot <- eva_data_main %>%
    #     filter(name %in%
    #              if(map_selections$preset == "Environmental justice") {
    #                metadata[metadata$ej == 1, ]$name
    #              } else if(map_selections$preset == "Climate change") {
    #                metadata[metadata$cc == 1, ]$name
    #              } else if(map_selections$preset == "Public health") {
    #                metadata[metadata$ph == 1, ]$name
    #              } else if(map_selections$preset == "Conservation") {
    #                metadata[metadata$cons == 1, ]$name
    #              } else if(map_selections$preset == "Custom") {
    #                c(map_selections$allInputs$value)}) %>%
    #     mutate(flag = if_else(tract_string %in%
    #                             if (geo_selections$selected_geo == "ctus") {
    #                               c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == param_area(), ]$tract_id)
    #                             } else if (geo_selections$selected_geo == "nhood") {
    #                               c(nhood_crosswalk[nhood_crosswalk$GEO_NAME == param_area(), ]$tract_id)
    #                             } else {c(param_area())}, "selected", NA_character_)) %>%
    #     filter(flag == "selected") %>%
    #     add_column(order = 2) %>%
    #     # filter(!is.na(weights_scaled)) %>%
    #     bind_rows(ps)  %>%
    #     
    #     ggplot(aes(y = weights_scaled, x = forcats::fct_reorder(name, order, .desc = TRUE), col = tract_string, group = tract_string))+
    #     geom_point(col = "black",
    #                position = position_dodge(width = .2),
    #                na.rm = T) + 
    #       geom_line(col = "black", alpha = .2,
    #                 position = position_dodge(width = .2),
    #                 na.rm = T) +
    #       geom_point(color = councilR::colors$councilBlue,
    #                  pch = 8, size = 3,
    #                  na.rm = T,
    #                # stat = "identity",
    #                  position = position_dodge(width = .2),
    #                 data = metadata %>%
    #                   filter(name %in%
    #                            if(map_selections$preset == "Environmental justice") {
    #                              metadata[metadata$ej == 1, ]$name
    #                            } else if(map_selections$preset == "Climate change") {
    #                              metadata[metadata$cc == 1, ]$name
    #                            } else if(map_selections$preset == "Public health") {
    #                              metadata[metadata$ph == 1, ]$name
    #                            } else if(map_selections$preset == "Conservation") {
    #                              metadata[metadata$cons == 1, ]$name
    #                            } else {c(map_selections$allInputs$value)}) %>%  
    #                   # full_join(tibble(name = "Aggregated priority score"),
    #                   #           MEANSCALED = NA, by = 'name') %>%
    #                   add_column(tract_string = "rgn"),
    #                 aes(y = MEANSCALED, x = name)) +
    #     geom_line(col = councilR::colors$councilBlue,
    #                position = position_dodge(width = .2),
    #                data = metadata %>%
    #                  filter(name %in%
    #                           if(map_selections$preset == "Environmental justice") {
    #                             metadata[metadata$ej == 1, ]$name
    #                           } else if(map_selections$preset == "Climate change") {
    #                             metadata[metadata$cc == 1, ]$name
    #                           } else if(map_selections$preset == "Public health") {
    #                             metadata[metadata$ph == 1, ]$name
    #                           } else if(map_selections$preset == "Conservation") {
    #                             metadata[metadata$cons == 1, ]$name
    #                           } else {c(map_selections$allInputs$value)}) %>%  
    #                  # full_join(tibble(name = "Aggregated priority score"),
    #                  #           MEANSCALED = NA, by = 'name') %>%
    #                  add_column(tract_string = "rgn"),
    #                aes(y = MEANSCALED, x = name),
    #               na.rm = T) +
    #     councilR::council_theme() +
    #     ylim(c(0,10)) +
    #     scale_x_discrete(labels = function(x) str_wrap(x, width = 40))+
    #     theme(panel.grid.major.x = element_blank(),
    #           axis.title.y = element_blank()) +
    #     labs(y = "Score (out of 10,\nwhere 10 indicates higest priority)") +
    #     coord_flip() 
    #   
    #   return(plot)
    # })
    
    
    output$table_para <- renderUI({
      ns <- session$ns
      req(geo_selections$selected_area)
      tagList(
          para <- HTML(paste0( 
            "<br> Here is a table of the tracts with the highest priority score in ", param_area(), ". To get the raw values, and/or see the numbers for all of the tracts, please download the data file at the top of this report.  <br>")
          ) )
          return(para)
    })
    output$priority_table <- renderTable({
      req(geo_selections$selected_area)
      
      head(param_selectedtractvalues()%>%
                 as_tibble() %>%
                 select(tract_string, MEAN, RANK) %>%
                 mutate(MEAN = round(MEAN, 3))  %>%
             arrange(RANK) %>%
                 rename(`Tract ID` = tract_string,
                        `Priority score` = MEAN,
                        `Rank of priority score` = RANK), 
           n = 5)
    })
    
    # output$priority_table <- renderDataTable({
    #   req(geo_selections$selected_area)
    #   
    #   # param_dl_data() %>%
    #   #   arrange(`Aggregated priority score`)
    #   DT::datatable(param_selectedtractvalues()%>%
    #     as_tibble() %>%
    #     select(tract_string, MEAN, RANK) %>%
    #     mutate(MEAN = round(MEAN, 3)) %>%
    #     rename(`Tract ID` = tract_string,
    #            `Priority score` = MEAN,
    #            `Rank of priority score` = RANK),
    #     options = list(
    #       pageLength = 5))
    #   
    #   # output <- DT::datatable(data2,
    #   #               options = list(lengthMenu = c(5, 10), pageLength = 5))
    #   # return(data2,
    #   #        options = list(pageLength = 5))
    #   # DT::datatable(
    #   #   (param_selectedtractvalues %>%
    #   #   as_tibble() %>%
    #   #   select(tract_string, MEAN, RANK) %>%
    #   #   rename(`Tract ID` = tract_string,
    #   #          `Priority score` = MEAN,
    #   #          `Rank of priority score` = RANK
    #   #          )),
    #   # options = list(
    #   #   pageLength = 5))
    # })
    
    
    output$equity_para <- renderUI({
      ns <- session$ns
      req(TEST() != "")
      para <- HTML(paste0( 
            "Research shows that trees are unevenly distributed across communities. ", 
            # In particular, 
            "Neighborhoods with high BIPOC or low-income populations have less tree canopy. ",
            # "(",
            # a("MacDonald 2021",
            #   href = "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0249715",
            #   .noWS = "outside",
            #   target = "_blank"),
            #   ") as do historically redlined areas (", 
            # a("NPR news story, ",
            #   href = "https://www.npr.org/2020/01/14/795961381/racist-housing-practices-from-the-1930s-linked-to-hotter-neighborhoods-today",
            #   .noWS = "outside",
            #   target = "_blank"), 
            # a("Locke et al. 2021, ",
            #   href = "https://www.nature.com/articles/s42949-021-00022-0",
            #   .noWS = "outside",
            #   target = "_blank"), 
            # a("Namin et al. 2020",
            #   href = "https://www.sciencedirect.com/science/article/abs/pii/S0277953619307531?via%3Dihub",
            #   .noWS = "outside",
            #   target = "_blank"), 
            # "). Addressing inequity in tree canopy cover may reduce heat-related deaths by up to 25% (",
            # a("Sinha 2021",
            #   href = "https://www.fs.fed.us/nrs/pubs/jrnl/2021/nrs_2021_paramita_001.pdf",
            #   .noWS = "outside",
            #   target = "_blank"), 
            #   ").<br><br>",
            # "This is true in the Twin Cities. 
            "In the plot below, tracts within ", 
            param_area(),
            " are in green, and the regional trend is in blue."
          )
          )
          return(para)
    })
    
    output$equity_plot <- renderPlot({
      req(TEST() != "")
      
      equityplot <- eva_data_main %>% 
        filter(variable %in% c("pbipoc", "canopy_percent", "ndvi", "mdhhincnow")) %>%
        select(tract_string, variable, raw_value) %>%
        pivot_wider(names_from = variable, values_from = raw_value) %>%
        # mutate(flag = if_else(tract_string %in% c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == "Minneapolis", ]$tract_id), "selected", NA_character_))
        mutate(flag = if_else(tract_string %in% 
                                if (geo_selections$selected_geo == "ctus") {
                                  c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                                } else if (geo_selections$selected_geo == "nhood") {
                                  c(nhood_crosswalk[nhood_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                                } else {c(param_area())}, "selected", NA_character_))
      
        race_equity <- equityplot %>%
          ggplot(aes(x = pbipoc, y = canopy_percent)) + 
          geom_point(col = "grey40", alpha = .3, data = filter(equityplot, is.na(flag)), na.rm = T) + 
          geom_smooth(method = "lm", formula = 'y ~ x', fill = NA, col = councilR::colors$councilBlue, data = equityplot, na.rm = T) +
          geom_point(fill = councilR::colors$cdGreen, size = 5, col = "black", pch = 21, data = filter(equityplot, flag == "selected"), na.rm = T) + 
          councilR::council_theme() + 
          scale_x_continuous(labels = scales::percent_format(accuracy = 1)) + 
          scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
          labs(x = "BIPOC population\n(%)", y = "Tree canopy\ncoverage (%)")
        
        
        inc_equity <- equityplot%>%
          ggplot(aes(x = mdhhincnow, y = (canopy_percent))) + 
          geom_point(col = "grey40", alpha = .3, data = filter(equityplot, is.na(flag)), na.rm = T) + 
          geom_smooth(method = "lm",  formula = 'y ~ x', fill = NA, col = councilR::colors$councilBlue, na.rm = T) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
          scale_x_continuous(labels = scales::dollar_format(accuracy = 1)) + 
          geom_point(fill = councilR::colors$cdGreen, size = 5, col = "black", pch = 21, data = filter(equityplot, flag == "selected"), na.rm = T) + 
          councilR::council_theme() + 
          labs(x = "Median household income", y = "Tree canopy\ncoverage (%)")
        
        fig_equity <- cowplot::plot_grid(race_equity, inc_equity, nrow = 2, labels = "AUTO")
      
      return(fig_equity)
    })
    
    
    
    output$other_para <- renderUI({
      ns <- session$ns
      req(TEST() != "")
      tagList(
          HTML(paste0( 
            # "The goal of this section is present information about biodiversity, management challenges, and other considerations for managing the tree canopy.<br><br>",
            "Invasion by the Emerald ash borer (EAB) insect is a major threat to existing tree canopy. Data shows that EAB has infested ", 
            eab %>% sf::st_intersection(filter(if (geo_selections$selected_geo == "ctus") {
              ctu_list
            } else if (geo_selections$selected_geo == "nhood") {
              nhood_list} else {mn_tracts}, GEO_NAME == param_area())) %>% nrow(), " trees in ",
            param_area(), " (",
            a("Minnesota DNR",
              href = "https://mnag.maps.arcgis.com/apps/webappviewer/index.html?id=63ebb977e2924d27b9ef0787ecedf6e9",
              .noWS = "outside",
              target = "_blank"), 
            "). Please note that these data are not necessarily intended to identify every ash tree (infested or not), however this information may still be useful.<br><br>",
            "Regional information about considerations related to climate change, the biodiversity of the existing tree canopy, and others are given under the 'other resources' tab at top."
            # "Low biodiversity is another threat to the tree canopy in the region. And knowing which species can adapt to a changing climate. Over the last 100 years, our region has seen a decline in oak trees, and an increase in ash, elm, and maple trees (<a href = 'https://gisdata.mn.gov/dataset/biota-original-pls-bearing-trees' target = '_blank'>Almendinger 1997</a>, <a href = 'https://www.nrs.fs.fed.us/data/urban/state/city/?city=6#ufore_data' target = '_blank'>Davey Resource Group 2004</a>). 'Other' species make up a larger percent of the tree canopy today, but these species are mostly introduced species rather than a diverse assemblage of native species (as was the case before 1900). "
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
    
    
    # new priority plot with bars
    output$priority_plot <- renderPlot({
      req(TEST() != "")
      
      ps <- param_selectedtractvalues() %>%
        st_drop_geometry() %>%
        rename(weights_scaled = MEAN) %>%
        add_column(name = "Aggregated priority score") %>%
        add_column(order = 1)
      
      plot <- eva_data_main %>%
        filter(name %in%
                 if(map_selections$preset == "Environmental justice") {
                   metadata[metadata$ej == 1, ]$name
                 } else if(map_selections$preset == "Climate change") {
                   metadata[metadata$cc == 1, ]$name
                 } else if(map_selections$preset == "Public health") {
                   metadata[metadata$ph == 1, ]$name
                 } else if(map_selections$preset == "Conservation") {
                   metadata[metadata$cons == 1, ]$name
                 } else if(map_selections$preset == "Custom") {
                   c(map_selections$allInputs$value)}) %>%
        mutate(flag = if_else(tract_string %in%
                                if (geo_selections$selected_geo == "ctus") {
                                  c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                                } else if (geo_selections$selected_geo == "nhood") {
                                  c(nhood_crosswalk[nhood_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                                } else {c(param_area())}, "selected", NA_character_)) %>%
        filter(flag == "selected") %>%
        add_column(order = 2) %>%
        # filter(!is.na(weights_scaled)) %>%
        bind_rows(ps)  %>%
        add_column(grouping = "Selected area") %>%
        group_by(grouping, name, order) %>%
        summarise(TEST = mean(weights_scaled, na.rm = T),
                  SE = sd(weights_scaled, na.rm = T)/sqrt(n())) %>%
        
        full_join(metadata %>%
                    filter(name %in%
                             if(map_selections$preset == "Environmental justice") {
                               metadata[metadata$ej == 1, ]$name
                             } else if(map_selections$preset == "Climate change") {
                               metadata[metadata$cc == 1, ]$name
                             } else if(map_selections$preset == "Public health") {
                               metadata[metadata$ph == 1, ]$name
                             } else if(map_selections$preset == "Conservation") {
                               metadata[metadata$cons == 1, ]$name
                             } else {c(map_selections$allInputs$value)}) %>%
                    # full_join(tibble(name = "Aggregated priority score"),
                    #           MEANSCALED = NA, by = 'name') %>%
                    add_column(grouping = "Region average", 
                               order = 2) %>%
                    rename(TEST = MEANSCALED)) %>%
        
        ggplot(aes(y = TEST, x = forcats::fct_reorder(name, order, .desc = TRUE)
                   , fill = grouping))+
        geom_bar(#color = councilR::colors$councilBlue,
                   # pch = 8, size = 3,
                   na.rm = T,
                   width = .7,
                   stat = "identity",
                   position = position_dodge(width = .7)) +
        scale_fill_manual(values = c(councilR::colors$councilBlue, councilR::colors$cdGreen)) +
        geom_errorbar(aes(ymin = TEST - SE, ymax = TEST + SE), 
                      width = 0,
                      position = position_dodge(width = .7)) +
        councilR::council_theme() +
        ylim(c(0,10)) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 30))+
        theme(panel.grid.major.x = element_blank(),
              axis.title.y = element_blank(),
              legend.position = "bottom") +
        labs(y = "Score (out of 10,\nwhere 10 indicates higest priority)",
             fill = "") +
        coord_flip() 
      
      return(plot)
    })
    
    
    # resource para -----
    # output$resource_para <- renderUI({
    #   ns <- session$ns
    #   req(TEST() != "")
    #   tagList(
    #     HTML(paste0( 
    #       "Managing the tree canopy is a complex and important subject. Growing Shade is a unique tool offering users the ability to customize prioritization and see detailed maps of tree canopy gaps. It is under active development, so please check back or contact us for more details. However there are other tools which may be useful, and it should be noted that there are still many unanswered questions. The list below has been compiled as a starting point.<br><br>", 
    #       "Finally, our experience and research tells us that data cannot substitute for engagement with prioritized stakeholders to understand community-specific concerns or opportunities. The on-the-ground knowledge of residents and the expertise of practitioners are valuable sources of information necessary to enhance and refine the shared understanding of this data.<br><br>",
    #       "<strong>Tools</strong> - What additional tools and assessments exist to help prioritize where to plant trees and maintain tree canopy? <br>-	",
    #       a("American Forests Tree Equity Score project",
    #         href = "https://www.americanforests.org/our-work/tree-equity-score/",
    #         .noWS = "outside",
    #         target = "_blank"),
    #       "<br>- ", 
    #       a("Hennepin County, MN Tree Canopy Tree Planting Priority Areas",
    #         href = "https://gis-hennepin.opendata.arcgis.com/pages/tree-planting",
    #         .noWS = "outside",
    #         target = "_blank"),
    #       "<br>- ",
    #       a("City of Saint Paul Urban Tree Canopy Assessment 2011",
    #         href = "https://www.stpaul.gov/departments/parks-recreation/natural-resources/forestry/urban-tree-canopy-assessment",
    #         .noWS = "outside",
    #         target = "_blank"),
    #       "<br><br><strong>Information, Guides and Toolkits</strong> - Where can I lean more about the benefits provided by urban forests and learn how to build them?  <br>-	",
    #       a("Vibrant Cities Lab",
    #         href = "https://www.vibrantcitieslab.com/",
    #         .noWS = "outside",
    #         target = "_blank"),
    #       "<br><br><strong>Climate Change</strong> - Where can I find more information about climate change impacts in the Twin Cities? <br>- ",
    #       a("Climate Vulnerability Assessment by Metropolitan Council for the Twin Cities",
    #         href = "https://www.fs.usda.gov/sites/default/files/fs_media/fs_document/urbannatureforhumanhealthandwellbeing_508_01_30_18.pdf%22 %EF%BF%BDHYPERLINK %22https://metrocouncil.org/Communities/Planning/Local-Planning-Assistance/CVA.aspx",
    #         .noWS = "outside",
    #         target = "_blank"),
    #       "<br>- ",
    #       a("Extreme Heat Map Tool",
    #         href = "https://metrocouncil.maps.arcgis.com/apps/webappviewer/index.html?id=fd0956de60c547ea9dea736f35b3b57e",
    #         .noWS = "outside",
    #         target = "_blank"), 
    #       " by Metropolitian Council<br>- ",
    #       a("Extreme Heat Story Map",
    #         href = "https://metrocouncil.maps.arcgis.com/apps/MapJournal/index.html?appid=7d9cdd3929e9439bb5b25aa1186d5783",
    #         .noWS = "outside",
    #         target = "_blank"),
    #       " by Metropolitian Council",
    #       
    #       "<br><br><strong>Human Health</strong> - Where can I learn about the impacts of tree on human health? <br>- ",
    #       a("US Forest Service Report: Urban Nature for Human Health and Well-being 2018",
    #         href = "https://www.fs.usda.gov/sites/default/files/fs_media/fs_document/urbannatureforhumanhealthandwellbeing_508_01_30_18.pdf",
    #         .noWS = "outside",
    #         target = "_blank"),
    #       
    #       "<br><br><strong>What’s Next</strong> - What new projects are underway that could support and inform urban forests in the Twin Cities?  <br>- ",
    #       a("Urban LTER (Long-term Ecological Research) in the Twin Cities",
    #         href = "https://mspurbanlter.umn.edu/overview",
    #         .noWS = "outside",
    #         target = "_blank")
    #     )
    #     )
    #   )
    # })
    

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
  
    
    output$dl_data <- downloadHandler(
      filename = function() {paste0("GrowingShadeReport_", param_area(), "_", Sys.Date(), ".xlsx")},
      content = function(file) {writexl::write_xlsx(
        list(
          # "Metadata" = metadata %>%
          #      rbind(c(""), c("Please use caution if using Excel formatting. You may need to divide cells by 100 for Excel to recognize percents correctly.", "", ""), 
          #            c("This data is obviously not finished. If you are seeing this warning, please do not use!.", "", ""), 
          #            c("The interactive tool can be accessed at <https://metrotransitmn.shinyapps.io/growing-shade/>.", "", "")),
             "Raw Data" = (param_dl_data()) # "Counties" = (eva_data_main)
             # #i'll probably want something like this
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
             
             ),
        path = file)}
    )
    
    
    ####### put things into reactive uis ----------
    
    output$tree_title <- renderUI({
      req(TEST() != "")
      h4("Tree canopy: ")})
    
    output$priority_title <- renderUI({
      req(TEST() != "")
      h4("Priortization: ")})
    
    output$equity_title <- renderUI({
      req(TEST() != "")
      h4("Equity: ")})
    
    output$other_title <- renderUI({
      req(TEST() != "")
      h4("Threats: ")})
    
    # output$resource_title <- renderUI({
    #   req(TEST() != "")
    #   h4("Other resources")})
    
    
    output$get_tree_plot <- renderUI({
      req(TEST() != "")
      plotOutput(ns("tree_plot"), "200px", width = "100%") %>%
        shinyhelper::helper(type = "markdown", content = "LineplotHelp", size = "m") })
    
    output$get_rank_plot <- renderUI({
      req(TEST() != "")
      plotOutput(ns("rank_plot"), "100px", width = "100%")})
    
    output$get_priority_plot <- renderUI({
      req(TEST() != "")
      plotOutput(ns("priority_plot"), "400px", width = "100%")})
    
    output$get_equity_plot <- renderUI({
      req(TEST() != "")
      plotOutput(ns("equity_plot"), "400px", width = "80%")})
    
    output$get_other_plot <- renderUI({
      req(TEST() != "")
      plotOutput(ns("other_plot"), "300px", width = "80%")})
    

    output$get_the_report <- renderUI({
      req(TEST() != "")
      downloadButton(ns('dl_report'), label = 'Comprehensive report') })
    
    
    output$get_the_data <- renderUI({
      req(TEST() != "")
      downloadButton(ns('dl_data'), label = 'Raw data') })
        
        
  })
}
    
## To be copied in the UI
# mod_report_ui("report_ui_1")
    
## To be copied in the server
# mod_report_server("report_ui_1")
