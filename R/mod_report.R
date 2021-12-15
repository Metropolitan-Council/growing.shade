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
    shinyWidgets::useShinydashboard(),

    fluidRow(column(width = 6, uiOutput(ns("get_the_report"))),
             column(width = 6,uiOutput(ns("get_the_data"))))
    
    ,(uiOutput(ns("geoarea")))
    , br(),
            
    fluidRow(shinydashboard::box(title = ("Tree canopy: "),
                        width = 12, collapsed = F,
                        status = "danger", solidHeader = F, collapsible = TRUE,
    uiOutput(ns("tree_para")),
      uiOutput(ns("get_tree_plot")))),

    fluidRow(shinydashboard::box(title = "Priortization: ",
                                 width = 12, collapsed = F,
                                 status = "danger", solidHeader = F, collapsible = TRUE,
                                 uiOutput(ns("rank_para")),
                                 uiOutput(ns("get_rank_plot")),
                                 br(),
                                 tableOutput(ns("priority_table")))),

    fluidRow(shinydashboard::box(title = "Equity: ",
                                 width = 12, collapsed = F,
                                 status = "danger", solidHeader = F, collapsible = TRUE,
    uiOutput(ns("equity_para")),
    uiOutput(ns("get_equity_plot")))),

    fluidRow(shinydashboard::box(title = "Threats: ",
                                 width = 12, collapsed = F,
                                 status = "danger", solidHeader = F, collapsible = TRUE,
    uiOutput(ns("other_para"))))
    )
    
}
    
#' report Server Functions
#'
#' @noRd 
#' @import ggplot2
#' @import tidyr
#' @import tibble
#' @import stringr
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
    
    
    param_areasummary <- reactive({
      req(TEST() != "")
      output <- if (geo_selections$selected_geo == "ctus") {
        ctu_list[ctu_list$GEO_NAME == param_area(), ]
      } else if (geo_selections$selected_geo == "nhood") {
        nhood_list[nhood_list$GEO_NAME == param_area(), ]
      } else if (geo_selections$selected_geo == "tracts") {
        mn_tracts[mn_tracts$GEOID == param_area(), ]
      }
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
    
    param_equity <- reactive({
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
      return(equityplot)
    })
    
    param_dl_data <- reactive({
      req(geo_selections$selected_area)
      # ps <- param_selectedtractvalues() %>%
      #   st_drop_geometry() %>%
      #   rename(raw_value = MEAN) %>%
      #   add_column(name = "Aggregated priority score") %>%
      #   add_column(order = 1)
      
      output <- eva_data_main %>%
        mutate(flag = if_else(tract_string %in%
                                if (geo_selections$selected_geo == "ctus") {
                                  c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                                } else {
                                  c(nhood_crosswalk[nhood_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                                }, "selected", NA_character_)) %>%
        filter(flag == "selected") %>%
        select(-flag)
      
      return(output)
    })
    
    
    param_test <- reactive({
      req(geo_selections$selected_area)
    })
    
    param_fancytract <- reactive({
      req(geo_selections$selected_geo == "tracts")
     fancyname <-  
        paste0(if (substr(param_area(), 3, 5) == "053") {"Hennepin County tract "
        } else if (substr(param_area(), 3, 5) == "003") {"Anoka County tract "
        } else if (substr(param_area(), 3, 5) == "019") {"Carver County tract "
        } else if (substr(param_area(), 3, 5) == "037") {"Dakota County tract "
        } else if (substr(param_area(), 3, 5) == "123") {"Ramsey County tract "
        } else if (substr(param_area(), 3, 5) == "139") {"Scott County tract "
        } else if (substr(param_area(), 3, 5) == "163") {"Washington County tract "}, 
        as.numeric(substr(param_area(), 6, 11))/100)
     return(fancyname)
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
        HTML(paste0("<h3>Growing Shade report for ", 
                    if(geo_selections$selected_geo == "tracts") {param_fancytract()} else {param_area()}, "</h3>"))
      )
    })    

    tree_text <- reactive({
      req(TEST() != "")
      tagList(HTML(
        paste0(
          if(geo_selections$selected_geo == "tracts") {
            paste0(param_fancytract(), " has an existing tree canopy coverage of ", param_areasummary()$canopy_percent, 
                   "% in 2020. Compared to other tracts across the region, the tree canopy in ", param_area(), " is ",
                   if(param_areasummary()$canopy_percent > (param_areasummary()$avgcanopy + .02)) {"above"
                   } else if(param_areasummary()$canopy_percent < (param_areasummary()$avgcanopy - .02)) {"below"
                   } else {"about equal to"}, 
                   " average (", round(param_areasummary()$avgcanopy*100, 1), "%).<br><br> ", 
                   "The distribution of tree canopy coverage across all of the region's tracts is shown below; the selected tract is highlighted in green.")
          } else { 
            paste0(param_area(),
                   " has an existing tree canopy coverage of ", round(param_areasummary()$canopy_percent*100, 1), 
                   "% in 2020. Compared to other ", if(geo_selections$selected_geo == "ctus") {"cities and townships"} else {"neighborhoods"},
                   " across ", if(geo_selections$selected_geo == "ctus") {"the region"} else (param_areasummary()$city),
                   ", the tree canopy in ", param_area(), " is ",
                   if(param_areasummary()$canopy_percent > (param_areasummary()$avgcanopy + .02)) {"above"
                   } else if(param_areasummary()$canopy_percent < (param_areasummary()$avgcanopy - .02)) {"below"
                   } else {"about equal to"}, 
                   " average (", round(param_areasummary()$avgcanopy*100, 1), "%). ",
                   "Within " , param_area(), ", there are ",
                   param_areasummary()$ntracts,
                   " Census tracts with tree canopy cover ranging from ",
                   param_areasummary()$min,
                   "% to ",
                   param_areasummary()$max,
                   "%. <br><br>The distribution of tree canopy is shown below. The selected area is highlighted in green. Census tracts have different areas and may overlap with other geographies, thus the exisiting tree canopy cover in the selected area may not be the mean of the tract canopy covers.")}
        )
      ))
    })
    
    output$tree_para <- renderUI({
      req(TEST() != "")
      HTML(paste0(tree_text(), " Read the methods in the 'other resources' tab to understand why our canopy cover numbers may differ from other tools."))
    })
    
    
    tree_report_plot <- reactive({
      req(TEST() != "")
      if(geo_selections$selected_geo != "tracts") {
        canopyplot<- eva_data_main %>%
          filter(variable %in% c("canopy_percent")) %>%
          select(tract_string, variable, raw_value) %>%
          mutate(flag = if_else(tract_string %in% 
                                  if (geo_selections$selected_geo == "ctus") {
                                    c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                                  } else if (geo_selections$selected_geo == "nhood") {
                                    c(nhood_crosswalk[nhood_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                                  } else {c(param_area())}, "selected", NA_character_)) %>%
          mutate(type = paste0(" Tracts\nwithin ", param_area()),
                 t2 = "tracts") %>%
          filter(flag == "selected") %>%
          bind_rows(as_tibble(if(geo_selections$selected_geo == "ctus") {ctu_list} else {nhood_list}) %>%
                      mutate(flag = if_else(GEO_NAME == param_area(), "selected", NA_character_)) %>%
                      rename(tract_string = GEO_NAME,
                             raw_value = canopy_percent) %>%
                      select(tract_string, raw_value, flag) %>%
                      mutate(variable = "canopy_percent",
                             type = if(geo_selections$selected_geo == "ctus") {"Cities across\nthe region"} else {paste0("Neighborhoods across\n", param_areasummary()$city)}))
      } else {
        canopyplot <- eva_data_main %>%
          filter(variable %in% c("canopy_percent")) %>%
          select(tract_string, variable, raw_value) %>%
          mutate(flag = if_else(tract_string %in%
                                  if (geo_selections$selected_geo == "ctus") {
                                    c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                                  } else if (geo_selections$selected_geo == "nhood") {
                                    c(nhood_crosswalk[nhood_crosswalk$GEO_NAME == param_area(), ]$tract_id)
                                  } else {c(param_area())}, "selected", NA_character_))
      }
      
      if(geo_selections$selected_geo != "tracts") {
        plot <- ggplot()+
          geom_boxplot(data = canopyplot, aes(x = raw_value, y = type),
                       outlier.shape = NA,
                       na.rm = T) +
          councilR::council_theme() +
          theme(panel.grid.minor = element_blank(),
                panel.grid.major.y = element_blank(),
                axis.text.y = element_text(size = 12)
          ) +
          ggtitle(paste0(param_area(), " tree canopy"))+
          geom_point(size = 1.3, alpha = .3,
                     position = position_jitter(seed = 1, width = 0, height = .3),
                     col = "grey40",
                     aes(x = raw_value, y = type),
                     data = filter(canopyplot, is.na(flag)),
                     na.rm = T) +
          labs(y = "", x = "Tree canopy cover (%)") +
          scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
          geom_point(aes(x = raw_value, y = type),
                     fill = councilR::colors$cdGreen,
                     size = 4, col = "black", pch = 21,
                     data = filter(canopyplot, flag == "selected", is.na(t2))) +
          geom_jitter(aes(x = raw_value, y = type),
                      position = position_jitter(seed = 1, width = 0, height = .3),
                      fill = councilR::colors$cdGreen,
                      size = 4, col = "black", pch = 21,
                      data = filter(canopyplot, flag == "selected", t2 == "tracts"),
                      na.rm = T)
      } else {
        plot<- ggplot() +
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
                axis.text.y = element_blank()
          ) +
          ggtitle(paste0(param_fancytract(), " tree canopy"))+
          geom_point(size = 1.3,alpha = .3,
                     position = position_jitter(seed = 1, width = 0, height = .3),
                     col = "grey40",
                     aes(x = raw_value, y = 1),
                     data = filter(canopyplot, is.na(flag)),
                     na.rm = T) +
          labs(y = "", x = "Tree canopy cover (%)") +
          scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
          geom_point(aes(x = raw_value, y = 1),
                     fill = councilR::colors$cdGreen,
                     size = 5, col = "black", pch = 21,
                     data = filter(canopyplot, flag == "selected"),
                     na.rm = T)
      }
      return(plot)
    })

    output$tree_plot <- renderPlot({
      req(TEST() != "")
      tree_report_plot()
    }#, res = 150)
    )
    # ranking section ------------
    
    rank_text <- reactive({
      req(TEST() != "")
      tagList(HTML(
        paste0( 
        "Using the ",
        tolower(map_selections$preset), 
        " preset, ", 
        if (geo_selections$selected_geo == "tracts") {
          paste0(param_fancytract(), " has a priority score of ", 
                 round((param_selectedtractvalues()$MEAN), 2), 
                 " (out of 10, where 10 indicates the highest priority) with a region-wide ranking of ", 
                 (param_selectedtractvalues()$RANK), " (out of 704 total tracts across the region). A plot of the tract rankings for all presets is shown below. A table containing the raw values of the variables used in the selected preset (", 
                 tolower(map_selections$preset), ") is also shown below. In the table, the average values for the selected area are compared to the region-wide averages.<br><br>")
        } else {paste0("tracts within ",
                       param_area(),
                       " have overall priority scores ranging from ",
                       round(min(param_selectedtractvalues()$MEAN), 2), " to ", round(max(param_selectedtractvalues()$MEAN), 2),
                       " and a region-wide ranking from ",
                       min(param_selectedtractvalues()$RANK), " to ", max(param_selectedtractvalues()$RANK), " (where a higher rank (closer to 1) indicates higher priorities). A plot of the tract rankings for all presets is shown below. A table containing the raw values of the variables used in the selected preset (", 
                       tolower(map_selections$preset), ") is also shown below. In the table, the average values for the selected area are compared to the region-wide averages.<br><br>")}
      )
      ))
    })
    
    
    output$rank_para <- renderUI({
      ns <- session$ns
      req(TEST() != "")
      rank_text()
    })
    
    report_rank_plot <- reactive({
      req(TEST() != "")
      test2 <- if (map_selections$preset != "Custom") {tibble()
      } else {
        param_selectedtractvalues() %>%
          rename(rank = RANK) %>%
          mutate(priority = " Custom")
      }
      
      segment_line <- if (map_selections$preset != "Custom") {
        tibble(y = c("Public health", "Environmental justice", "Conservation", "Climate change"))
      } else {
        tibble(y = c("Public health", "Environmental justice", "Conservation", "Climate change", " Custom"))
      }
      
      test <- param_selectedtractvalues() %>%
        # mn_tracts %>% filter(GEO_NAME == "27037060716") %>%#
        st_drop_geometry()  %>%
        select(`Public health`, Conservation, `Environmental justice`, `Climate change`, GEO_NAME) %>%
        pivot_longer(names_to = "priority", values_to = "rank", -GEO_NAME) %>%
        bind_rows(test2)
      
      plot <- #if (map_selections$priority_layer == "Off") {print("nothing to see here")
        # } else {
        ggplot() +
        scale_x_continuous( limits = c(1, 704), labels = c(1, 250, 500, 704), breaks = c(1, 250, 500, 704)) +
        geom_errorbarh(data = test,aes(xmax = rank, xmin = rank, y = forcats::fct_rev(priority)), height = .5) +
        councilR::council_theme() +
        theme(#axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
        geom_segment(aes(x = 1, xend = 700, y = segment_line$y, 
                         yend = segment_line$y)) +
        labs(x = "Rank of aggregated priority score\n(out of 704 tracts across the region)") 
      # }
      return(plot)
      
    })
    
    output$rank_plot <- renderPlot({
      req(TEST() != "")
      report_rank_plot()
    })
    
    # priority section -----------
    
    output$priority_table <- renderTable({
      # report_priority_table()
      req(geo_selections$selected_area)
      req(TEST() != "")

        x <- eva_data_main %>%
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
          # filter(!is.na(raw_value)) %>%
          # bind_rows(ps)  %>%
          add_column(grouping = "Selected area") %>%
          group_by(grouping, name, order) %>%
          summarise(RAW = mean(raw_value, na.rm = T),
                    SE = sd(raw_value, na.rm = T)/sqrt(n())) %>%

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
                      rename(RAW = MEANRAW)) %>%
          ungroup() %>%
          select(grouping, name, RAW) %>% #, SE) %>%
          filter(!is.na(name)) %>%

          pivot_wider(names_from = grouping, values_from = RAW) %>%
          rename(Variable = name) %>%
          mutate(`Region average` = case_when(str_detect(`Variable`, "%") ~ paste0(round(`Region average` * 100, 2), "%"),
                                             TRUE ~ as.character(round(`Region average`, 2)))) %>%

          mutate(`Selected area` = case_when(str_detect(`Variable`, "%") ~ paste0(round(`Selected area` * 100, 2), "%"),
                                             TRUE ~ as.character(round(`Selected area`, 2))))

      return(x)
    })
    
 
    
    
    output$equity_para <- renderUI({
      ns <- session$ns
      req(TEST() != "")
      para <- HTML(paste0( 
            "Research shows that trees are unevenly distributed across communities. ", 
            # In particular, 
            "Areas with high BIPOC or low-income populations have less tree canopy. ",
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
            "In the plot below, ",
            if (geo_selections$selected_geo == "tracts") {
              paste0(param_fancytract(), " is ")
              } else {paste0("tracts within ", 
            param_area(),
            " are ")},
            "in green, and the regional trend is in blue."
          )
          )
          return(para)
    })
    

    
    output$equity_plot <- renderPlot({
      req(TEST() != "")
      race_equity <- param_equity() %>%
        ggplot(aes(x = pbipoc, y = canopy_percent)) + 
        geom_point(col = "grey40", alpha = .3, data = filter(param_equity(), is.na(flag)), na.rm = T) + 
        geom_smooth(method = "lm", formula = 'y ~ x', fill = NA, col = councilR::colors$councilBlue, data = param_equity(), na.rm = T) +
        geom_point(fill = councilR::colors$cdGreen, size = 5, col = "black", pch = 21, data = filter(param_equity(), flag == "selected"), na.rm = T) + 
        councilR::council_theme() + 
        theme(panel.grid.minor = element_blank(),
              panel.grid.major = element_blank()) +
        scale_x_continuous(labels = scales::percent_format(accuracy = 1)) + 
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
        labs(x = "BIPOC population\n(%)", y = "Tree canopy\n (%)")
      
      inc_equity <- param_equity()%>%
        ggplot(aes(x = mdhhincnow/1000, y = (canopy_percent))) + 
        geom_point(col = "grey40", alpha = .3, data = filter(param_equity(), is.na(flag)), na.rm = T) + 
        geom_smooth(method = "lm",  formula = 'y ~ x', fill = NA, col = councilR::colors$councilBlue, na.rm = T) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
        scale_x_continuous(labels = scales::dollar_format(accuracy = 1)) + 
        geom_point(fill = councilR::colors$cdGreen, size = 5, col = "black", pch = 21, data = filter(param_equity(), flag == "selected"), na.rm = T) + 
        councilR::council_theme() + 
        theme(panel.grid.minor = element_blank(),
              panel.grid.major = element_blank()) +
        labs(x = "Median household income\n($, thousands)", y = "Tree canopy\n (%)")# +
        # theme(axis.title.y = element_blank(),
        #       axis.text.y = element_blank())
      fig_equity <- cowplot::plot_grid(race_equity, inc_equity, nrow = 2, labels = "AUTO")
      return(fig_equity)
    })
    
    
    
    output$other_para <- renderUI({
      ns <- session$ns
      req(TEST() != "")
      tagList(
          HTML(paste0( 
            # "The goal of this section is present information about biodiversity, management challenges, and other considerations for managing the tree canopy.<br><br>",
            "The Emerald ash borer (EAB) insect is a major threat to existing tree canopy. Data shows that EAB has infested ", 
            param_areasummary()$EAB, " trees in ",
            if (geo_selections$selected_geo == "tracts") {
              param_fancytract()} else {param_area()}, " (",
            a("Minnesota DNR",
              href = "https://mnag.maps.arcgis.com/apps/webappviewer/index.html?id=63ebb977e2924d27b9ef0787ecedf6e9",
              .noWS = "outside",
              target = "_blank"), 
            "). Please note that these data are not necessarily intended to identify every ash tree (infested or not), however this information may still be useful.<br><br>",
            "Regional information about considerations related to climate change, the biodiversity of the existing tree canopy, and others are given under the 'other resources' tab at top.<br><br>"
            # "Low biodiversity is another threat to the tree canopy in the region. And knowing which species can adapt to a changing climate. Over the last 100 years, our region has seen a decline in oak trees, and an increase in ash, elm, and maple trees (<a href = 'https://gisdata.mn.gov/dataset/biota-original-pls-bearing-trees' target = '_blank'>Almendinger 1997</a>, <a href = 'https://www.nrs.fs.fed.us/data/urban/state/city/?city=6#ufore_data' target = '_blank'>Davey Resource Group 2004</a>). 'Other' species make up a larger percent of the tree canopy today, but these species are mostly introduced species rather than a diverse assemblage of native species (as was the case before 1900). "
          )
          )
      )
    })
    
    
    # output$other_plot <- renderPlot({
    #   treebiodiv %>%
    #     ggplot(aes(x = (timepoint), y = percent, fill = spp_name, shape = spp_name)) +
    #     geom_line( position = position_dodge(width = 10))+#, aes(col = spp_name)) +
    #     geom_point(
    #       size = 5, position = position_dodge(width = 10)) + 
    #     scale_fill_brewer(palette = "Paired", name = "Species") +
    #     scale_color_brewer(palette = "Paired", name = "Species") +
    #     scale_shape_manual(values = rep(c(21:25), 3), name = "Species")+
    #     councilR::council_theme() +
    #     labs(x = "Year", y = "Species composition (%)") +
    #     guides(fill = guide_legend(nrow = 6, byrow = T),
    #            color = guide_legend(nrow = 6, byrow = T),
    #            shape = guide_legend(nrow = 6, byrow = T))
    #   
    # })
    # 
    
    
    output$dl_report <- downloadHandler(
      filename = paste0("GrowingShade_", Sys.Date(), ".html"),
      content = function(file) {
        tempReport <- file.path(tempdir(), "report_new.Rmd")
        file.copy("report_new.Rmd", tempReport, overwrite = TRUE)
        # Set up parameters to pass to Rmd document
        params <- list(param_geo = geo_selections$selected_geo,
                       param_area = param_area(),
                       param_equitypara = tree_text(),
                       param_treeplot = tree_report_plot(),
                       param_ranktext = rank_text(),
                       param_rankplot = report_rank_plot()#,
                       # param_prioritytable = report_priority_table(),

                       
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
          "Metadata" = #tibble() %>% #metadata %>%
            # select(-type, -n, -niceinterp, -nicer_interp, -interpret_high_value) %>%
            # rename(`Variable description` = name) %>%
            # filter(!is.na(`Variable description`)) %>%
               rbind(#c(""),
                     c("Please use caution if using Excel formatting. You may need to divide cells by 100 for Excel to recognize percents correctly.", "", ""),
                     c("This data is obviously not finished. If you are seeing this warning, please do not use!.", "", ""),
                     c("The interactive tool can be accessed at <https://metrotransitmn.shinyapps.io/growing-shade/>.", "", "")),
          "Summarized Data" = param_selectedtractvalues(),
             "Raw Data" = (param_dl_data() %>%
                             filter(!is.na(name)) %>%
                             select(tract_string, name, raw_value, weights_scaled, overall_rank) %>%
                             rename(GEOID = tract_string,
                                    `Variable description` = name,
                                    `Raw value` = raw_value,
                                    `Scaled and centered score`= weights_scaled,
                                    `Rank of score` = overall_rank)) # "Counties" = (eva_data_main)
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

    
    output$get_tree_plot <- renderUI({
      req(TEST() != "")
      plotOutput(ns("tree_plot"), "200px", width = "100%") %>%
        shinyhelper::helper(type = "markdown", content = "LineplotHelp", size = "m") })
    
    output$get_rank_plot <- renderUI({
      req(TEST() != "")
      plotOutput(ns("rank_plot"), "300px", width = "100%") %>%
        shinyhelper::helper(type = "markdown", content = "RankHelp", size = "m") })
    
    
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
