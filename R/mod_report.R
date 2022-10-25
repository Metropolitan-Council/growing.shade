#' report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # shinyjs::useShinyjs(),
    shinybrowser::detect(),
    
    shinyWidgets::useShinydashboard(),
    # ,
    (uiOutput(ns("geoarea"))),
    br(),
    fluidRow(uiOutput(ns("treecanopy_box"))),
    fluidRow(uiOutput(ns("priority_box"))),
    fluidRow(uiOutput(ns("disparity_box"))),
    fluidRow(uiOutput(ns("temp_box"))), 
    fluidRow(uiOutput(ns("download_box")))
  )
}

#' report Server Functions
#'
#' @noRd
#' @import ggplot2
#' @import tidyr
#' @import tibble
#' @import stringr
#' @import ggbeeswarm
#' @import ggtext
#' @import councilR
mod_report_server <- function(id,
                              geo_selections,
                              map_selections,
                              blockgroup_selections,
                              map_util) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    library(councilR)

    ####### things to export
    TEST <- reactive({
      TEST <- if (geo_selections$selected_geo == "ctus") {
        geo_selections$selected_area
      } else if (geo_selections$selected_geo == "nhood") {
        geo_selections$selected_area
      } else if (geo_selections$selected_geo == "blockgroups") {
        blockgroup_selections$selected_blockgroup
      }
      return(TEST)
    })

    param_area <- reactive({
      req(TEST() != "")
      output <- TEST()
      return(output)
    })


    # the min, max, n_blockgroups, eab, treeacres, landacres, canopypercent, avgcanopy for the selected geography
    param_areasummary <- reactive({
      req(TEST() != "")
      output <- if (geo_selections$selected_geo == "ctus") {
        sf::st_drop_geometry(ctu_list[ctu_list$GEO_NAME == param_area(), ])
      } else if (geo_selections$selected_geo == "nhood") {
        sf::st_drop_geometry(nhood_list[nhood_list$GEO_NAME == param_area(), ])
      } else if (geo_selections$selected_geo == "blockgroups") {
        sf::st_drop_geometry(mn_bgs[mn_bgs$GEOID == param_area(), ])
      }
      return(output)
    })

    # the min/max/other data for all blockgroups within a given ctu/nhood/blockgroup (n = 1 for blockgroups, n > 1 for most ctus/nhoods)
    param_selectedblockgroupvalues <- reactive({
      req(TEST() != "")
      output <- filter(
        (map_util$map_data),
        bg_string %in%
          if (geo_selections$selected_geo == "ctus") {
            c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == param_area(), ]$bg_id)
          } else if (geo_selections$selected_geo == "nhood") {
            c(nhood_crosswalk[nhood_crosswalk$GEO_NAME == param_area(), ]$bg_id)
          } else {
            c(param_area())
          }
      )
      return(output)
    })
    
    selected_length <- reactive({
      req(TEST() != "")
      nrow(param_selectedblockgroupvalues())
    })

    # all data with flag for selected areas
    param_dl_data <- reactive({
      req(TEST() != "")

      output <- bg_growingshade_main %>%
        mutate(flag = if_else(bg_string %in%
          if (geo_selections$selected_geo == "ctus") {
            c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == param_area(), ]$bg_id)
          } else if (geo_selections$selected_geo == "nhood") {
            c(nhood_crosswalk[nhood_crosswalk$GEO_NAME == param_area(), ]$bg_id)
          } else if (geo_selections$selected_geo == "blockgroups") {
            c(param_area())
          },
        "selected", NA_character_
        ))
      return(output)
    })

    param_equity <- reactive({
      equityplot <- param_dl_data() %>%
        filter(variable %in% c("pbipoc", "canopy_percent", "hhincome", "avg_temp", "ndvi_land")) %>%
        select(bg_string, variable, raw_value, flag) %>%
        pivot_wider(names_from = variable, values_from = raw_value)
      return(equityplot)
    })

    output$geoarea <- renderUI({
      ns <- session$ns
      tagList(
        HTML(paste0(
          "<h2><section style='font-size:20pt'>Growing Shade report for ",
          if (geo_selections$selected_geo == "blockgroups") {
            param_areasummary()$fancyname
          } else {
            param_area()
          }, "</h2></section>"
        ))
      )
    })

    tree_text <- reactive({
      req(TEST() != "")
      tagList(HTML(
        paste0(
          if (geo_selections$selected_geo == "blockgroups") {
            paste0(
              param_areasummary()$fancyname, " has an existing tree canopy coverage of ", round(param_areasummary()$canopy_percent * 100, 2),
              "% in 2021. Compared to other block groups across the region, the tree canopy in the selected block group is ",
              if (param_areasummary()$canopy_percent > (param_areasummary()$avgcanopy + .02)) {
                "above"
              } else if (param_areasummary()$canopy_percent < (param_areasummary()$avgcanopy - .02)) {
                "below"
              } else {
                "about equal to"
              },
              " average (", round(param_areasummary()$avgcanopy * 100, 1), "%).<br><br> ",
              "The plot below shows how tree canopy cover in the selected block group (shown in green) compares to other block groups across the region. In most areas, a goal of 45% tree canopy coverage (as detected by our methods) is suitable."
            )
          } else {
            paste0(
              param_area(),
              " has an existing tree canopy coverage of ", round(param_areasummary()$canopy_percent * 100, 1),
              "% in 2021. Compared to other ", if (geo_selections$selected_geo == "ctus") {
                "cities and townships"
              } else {
                "neighborhoods"
              },
              " across ", if (geo_selections$selected_geo == "ctus") {
                "the region"
              } else {
                (param_areasummary()$city)
              },
              ", the tree canopy in ", param_area(), " is ",
              if (param_areasummary()$canopy_percent > (param_areasummary()$avgcanopy + .02)) {
                "above"
              } else if (param_areasummary()$canopy_percent < (param_areasummary()$avgcanopy - .02)) {
                "below"
              } else {
                "about equal to"
              },
              " average (", round(param_areasummary()$avgcanopy * 100, 1), "%). ",
              "Within ", param_area(), ", there are ",
              param_areasummary()$n_blockgroups,
              " Census block groups with tree canopy cover ranging from ",
              param_areasummary()$min,
              "% to ",
              param_areasummary()$max,
              "%. <br><br>The plot below shows how tree canopy cover in the selected area (shown in green) compares to other areas across the region. Within the selected area, tree canopy cover varies across census block groups. In most areas, a goal of 45% tree canopy coverage (as detected by our methods) is suitable."
            )
          }
        )
      ))
    })

    # output$tree_para <- renderUI({
    #   req(TEST() != "")
    #   (tree_text())
    #   # HTML(paste0(tree_text(), " Read the methods in the 'other resources' tab to understand why our canopy cover numbers may differ from other tools."))
    # })


    report_tree_plot <- reactive({
      req(TEST() != "")
      set.seed(12345)
      if (geo_selections$selected_geo != "blockgroups") {
        canopyplot <-
          (as_tibble(if (geo_selections$selected_geo == "ctus") {
            ctu_list
          } else {
            nhood_list
          }) %>%
            mutate(flag = if_else(GEO_NAME == param_area(), "selected", NA_character_)) %>%
            rename(bg_string = GEO_NAME) %>%
            select(bg_string, canopy_percent, flag) %>%
            mutate(type = if (geo_selections$selected_geo == "ctus") {
              "Cities across\nthe region"
            } else {
              paste0("Neighborhoods across\n", param_areasummary()$city)
            })) %>%
          bind_rows(filter(param_equity(), flag == "selected") %>%
            mutate(
              t2 = "block groups",
              type = paste0(" Block groups\nwithin ", param_area())
            )) %>%
          rename(raw_value = canopy_percent)
      } else {
        canopyplot <- param_equity() %>%
          rename(raw_value = canopy_percent)
      }

      if (geo_selections$selected_geo != "blockgroups") {
        plot <- ggplot() +
          councilR::theme_council() +
          theme(
            plot.title = element_text(size = 16),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.text.y = element_text(size = 12),
            plot.caption = element_text(
              size = rel(1),
              colour = "grey30"
            )
          ) +
          ggbeeswarm::geom_beeswarm(
            size = 2.5, #if(geo_selections$selected_geo == "ctus") {2.5} else {3}, 
            alpha = .3,
            cex = 3,
            # corral = "wrap", corral.width = 0.5,
            method = "compactswarm",
            col = "grey40",
            aes(x = raw_value, y = type),
            data = filter(canopyplot, is.na(flag)),
            na.rm = T
          ) +
          labs(
            y = "", x = "Tree canopy cover (%)",
            caption = "\nSource: Analysis of Sentinel-2 satellite imagery (2021)"
          ) +
          scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
          geom_point(aes(x = raw_value, y = type),
            fill = councilR::colors$cdGreen,
            size = 4, col = "black", pch = 21, stroke = 1, 
            data = filter(canopyplot, flag == "selected", is.na(t2))
          ) +

          ggbeeswarm::geom_beeswarm(aes(x = raw_value, y = type),
                                    cex = if (selected_length() > 100) {2} else {3}, 
                                    stroke = if(selected_length() > 100) {0} else {1},
                                    size = if (selected_length() > 100) {2} else {3},
                                    corral = "wrap", corral.width = 0.7,
                                    fill = councilR::colors$cdGreen, 
                                    col = "black", pch = 21, alpha = .8,
                                    data = filter(canopyplot, flag == "selected", t2 == "block groups"),
                                    method = "compactswarm",
                                    na.rm = T
          ) 
        
      } else {
        plot <- ggplot() +
          councilR::theme_council() +
          theme(
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            plot.title = element_text(size = 16),
            axis.text.y = element_blank(), # element_text(size = 12),
            plot.caption = element_text(
              size = rel(1),
              colour = "grey30"
            )
          ) +
          ggbeeswarm::geom_quasirandom(
            groupOnX = F, varwidth = T,
            cex = 1, # size = 1.3,
            alpha = .3,
            col = "grey40",
            aes(x = raw_value, y = 1),
            data = filter(canopyplot, is.na(flag)),
            na.rm = T
          ) +

          labs(
            y = "", x = "Tree canopy cover (%)",
            caption = "\nSource: Analysis of Sentinel-2 satellite imagery (2021)"
          ) +
          scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
          geom_point(aes(x = raw_value, y = 1),
            fill = councilR::colors$cdGreen,
            size = 5, col = "black", pch = 21, stroke = 1, 
            data = filter(canopyplot, flag == "selected"),
            na.rm = T
          )
      }
      return(plot)
    })

    # output$tree_plot <- renderPlot(
    #   {
    #     req(TEST() != "")
    #     report_tree_plot()
    #   } # , res = 150)
    # )

    output$tree_plot <- renderImage(
      {
        req(TEST() != "")

        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- tempfile(fileext = ".png")

        # Generate the PNG
        png(outfile,
          width = 500 * 2,
          height = 300 * 2,
          res = 72 * 2
        )
        print(report_tree_plot())
        dev.off()

        # Return a list containing the filename
        list(
          src = outfile,
          contentType = "image/png",
          width = 500,
          height = 300,
          alt = "Figure showing the distribution of tree canopy across the region and within the selected geography."
        )
      },
      deleteFile = TRUE
    )

    # ranking section ------------

    rank_text <- reactive({
      req(TEST() != "")
      tagList(HTML(
        paste0(
          "Using the ",
          tolower(map_selections$preset),
          " layer, ", #signing test
          if (geo_selections$selected_geo == "blockgroups") {
            paste0(
              param_areasummary()$fancyname, 
              if(!is.na(param_selectedblockgroupvalues()$MEAN)) {paste0(" has a score of ", round((param_selectedblockgroupvalues()$MEAN), 2))} else {paste0("'s priority score cannot be computed due to insufficient data ")},
              " (where 10 indicates highest priority; distance between priority scores can be interpreted on a continuous, linear scale). Scores for all priority layers are shown below. A table compares the values of the variables used in the ",
              tolower(map_selections$preset), " priority layer between the selected area and region-wide averages.<br>"
            )
          } else {
            paste0(
              "block groups within ",
              param_area(),
              " have priority scores ranging from ",
              round(min(param_selectedblockgroupvalues()$MEAN, na.rm = T), 2), " to ", round(max(param_selectedblockgroupvalues()$MEAN, na.rm = T), 2),
              "  (where 10 indicates highest priority; distance between priority scores can be interpreted on a continuous, linear scale). Scores for all priority layers are shown below. A table compares the values of the variables used in the ",
              tolower(map_selections$preset), " priority layer between the selected area (average of ", param_areasummary()$n_blockgroups, " block group values) and region-wide averages.<br>"
            )
          }
        )
      ))
    })

    # output$rank_para <- renderUI({
    #   ns <- session$ns
    #   req(TEST() != "")
    #   rank_text()
    # })

    report_rank_plot <- reactive({
      req(TEST() != "")
      set.seed(12345)
      test2 <- if (map_selections$preset != "Custom") {
        tibble()
      } else {
        param_selectedblockgroupvalues() %>%
          # rename(score = MEAN) %>%
          mutate(priority = " Custom")
      }

      test <- param_selectedblockgroupvalues() %>%
        st_drop_geometry() %>%
        dplyr::select(`Public health`, Conservation, `Environmental justice`, `Climate change`, GEO_NAME) %>%
        pivot_longer(names_to = "priority", values_to = "score", -GEO_NAME) %>%
        bind_rows(test2)

      plot <-
        ggplot() +
        councilR::theme_council() +
        theme(
          axis.title.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.placement = "outside",
          axis.ticks.x = element_blank(), # element_line(),
          plot.caption = element_text(
            size = rel(1),
            colour = "grey30"
          )
        ) +
        scale_x_continuous(
          limits = c(0, 10),
          breaks = c(0, 2.5, 5, 7.5, 10),
          labels = c("0 (lowest\npriority)", 2.5, 5, 7.5, "10 (highest\npriority)")
        ) +
      ggbeeswarm::geom_beeswarm(aes(x = score, y = forcats::fct_rev(priority)),
                                   # groupOnX = F, varwidth = T,
                                   cex = if (selected_length() > 100) {2} else {3}, 
                                stroke = if(selected_length() > 100) {0} else {1},
                                size = if (selected_length() > 100) {2} else {3},
                                corral = "wrap", corral.width = 0.7,
                                   fill = councilR::colors$cdGreen, 
                                   col = "black", pch = 21, alpha = .8,
                                   data = test,
                                method = "compactswarm",
                                   na.rm = T
      ) +
        labs(
          x = "Block group priority scores\n(where 10 indicates highest priority)",
          caption = "\nSource: Analysis of Sentinel-2 satellite imagery (2021), ACS 5-year estimates (2016-2020),\ndecennial census (2020), and CDC PLACES data (2020)"
        )
      return(plot)
    })


    output$rank_plot <- renderImage(
      {
        req(TEST() != "")

        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- tempfile(fileext = ".png")

        # Generate the PNG
        png(outfile,
          width = 500 * 2,
          height = 300 * 2,
          res = 72 * 2
        )
        print(report_rank_plot())
        dev.off()

        # Return a list containing the filename
        list(
          src = outfile,
          contentType = "image/png",
          width = 500,
          height = 300,
          alt = "Figure showing the priority ranking (climate change, conservation, environmental justice, public health) for all block groups within the selected geography."
        )
      },
      deleteFile = TRUE
    )

    # priority section -----------

    report_priority_table <- reactive({
      req(TEST() != "")

      step1 <- param_dl_data() %>%
        filter(name %in%
          if (map_selections$preset == "Environmental justice") {
            metadata[metadata$environmental_justice == 1, ]$name
          } else if (map_selections$preset == "Climate change") {
            metadata[metadata$climate_change == 1, ]$name
          } else if (map_selections$preset == "Public health") {
            metadata[metadata$public_health == 1, ]$name
          } else if (map_selections$preset == "Conservation") {
            metadata[metadata$conservation == 1, ]$name
          } else if (map_selections$preset == "Custom") {
            c(map_selections$allInputs$value)
          }) %>%
        filter(flag == "selected") %>%
        add_column(order = 2) %>%
        # filter(!is.na(raw_value)) %>%
        # bind_rows(ps)  %>%
        add_column(grouping = "Selected area") %>%
        group_by(grouping, name, order) %>%
        summarise(
          RAW = mean(raw_value, na.rm = T),
          SE = sd(raw_value, na.rm = T) / sqrt(n())
        )

      x <- step1 %>%
        full_join(metadata %>%
          filter(name %in%
            if (map_selections$preset == "Environmental justice") {
              metadata[metadata$environmental_justice == 1, ]$name
            } else if (map_selections$preset == "Climate change") {
              metadata[metadata$climate_change == 1, ]$name
            } else if (map_selections$preset == "Public health") {
              metadata[metadata$public_health == 1, ]$name
            } else if (map_selections$preset == "Conservation") {
              metadata[metadata$conservation == 1, ]$name
            } else {
              c(map_selections$allInputs$value)
            }) %>%
          # full_join(tibble(name = "Aggregated priority score"),
          #           MEANSCALED = NA, by = 'name') %>%
          add_column(
            grouping = "Region average",
            order = 2
          ) %>%
          rename(RAW = MEANRAW),
        by = c("grouping", "name", "order", "RAW")
        ) %>%
        ungroup() %>%
        select(grouping, name, RAW) %>%
        # , SE) %>%
        filter(!is.na(name)) %>%
        pivot_wider(names_from = grouping, values_from = RAW) %>%
        rename(Variable = name) %>%
        mutate(`Region average` = case_when(
          str_detect(`Variable`, "%") ~ paste0(round(`Region average` * 100, 2), "%"),
          TRUE ~ as.character(round(`Region average`, 2))
        )) %>%
        mutate(`Selected area` = case_when(
          str_detect(`Variable`, "%") ~ paste0(round(`Selected area` * 100, 2), "%"),
          TRUE ~ as.character(round(`Selected area`, 2))
        ))

      return(x)
    })

    output$priority_table <- renderTable(striped = TRUE, {
      req(TEST() != "")
      report_priority_table()
    })

    equity_text <- reactive({
      ns <- session$ns
      req(TEST() != "")
      para <- HTML(paste0(
        "Research shows that trees are not distributed equitably across communities. Lower-income areas (<a href='https://doi.org/10.1371/journal.pone.0249715' target = '_blank'>McDonald et al. 2021</a>) and areas with more people identifying as persons of color (<a href = 'https://doi.org/10.1016/j.jenvman.2017.12.021' target='_blank'>Watkins and Gerris 2018</a>) have less tree canopy. Trends in our region are shown below; ",
        if (geo_selections$selected_geo == "blockgroups") {
          paste0(param_areasummary()$fancyname, " is ")
        } else {
          paste0(
            "block groups within ",
            param_area(),
            " are "
          )
        },
        "in green and the regional trend is in blue.<br><br>"
      ))
      return(para)
    })

    # output$equity_para <- renderUI({
    #   req(TEST() != "")
    #   (equity_text())
    # })

    # output$download_para <- renderUI({
    #   ns <- session$ns
    #   req(TEST() != "")
    #   para <- HTML(
    #     "Use the buttons below to download a version of this report which can be printed or shared. The raw data may also be downloaded as an excel or shapefile.<br>"
    #   )
    #   return(para)
    # })


    heat_text <- reactive({
      ns <- session$ns
      req(TEST() != "")
      para <- HTML(paste0(
        "Trees and other green space help cool temperatures. Temperature differences between moderate and high amounts of green space can be up to 10 degrees. Adding green space can reduce hundreds of heat-related deaths (<a href='https://www.fs.fed.us/nrs/pubs/jrnl/2021/nrs_2021_paramita_001.pdf' target = '_blank'>Sinha et al. 2021</a>). The impact of green space on temperature is shown below. ",
        if (geo_selections$selected_geo == "blockgroups") {
          paste0(param_areasummary()$fancyname, " is ")
        } else {
          paste0(
            "Block groups within ",
            param_area(),
            " are "
          )
        },
        "in green and the regional trend is in blue.<br><br>"
      ))
      return(para)
    })


    # output$heat_para <- renderUI({
    #   req(TEST() != "")
    #   heat_text()
    # })

    report_equity_plot <- reactive({
      req(TEST() != "")
      df <- param_equity() %>%
        select(flag, canopy_percent, hhincome, pbipoc) %>%
        pivot_longer(names_to = "names", values_to = "raw_value", -c(flag, canopy_percent)) %>%
        mutate(raw_value = if_else(names == "pbipoc", raw_value * 100, raw_value))

      # breaks_fun <- function(x) {
      #   if (max(x) < 101) {
      #   # if(name(x))
      #     seq(0, 1, .25)
      #   } else {
      #     seq(0, 250000, 75000)
      #     # seq(0, 250000, 100000)
      #   }
      # }
      # labs_fun <- function(x) {
      #   if (max(x) < 101) {
      #     c("0%", "25%", "50%", "75%", "100%")
      #   } else {
      #     c("$0", "$75,000", "$150,000", "$225,000")
      #     # c("$0", "$100,000", "200,000")
      #   }
      # }
      # df<-bg_growingshade_main %>%
      #   filter(variable %in% c("canopy_percent", "pbipoc", "hhincome")) %>%
      #   select(bg_string, variable, raw_value) %>%
      #   pivot_wider(names_from = variable,values_from = raw_value) %>%
      #   pivot_longer(names_to = "names", values_to = "raw_value", -c(bg_string, canopy_percent))
      #
      #   ggplot(aes(x = raw_value, y = canopy_percent), data = df) +
      #   geom_point(col = "grey40", alpha = .3,  na.rm = T) +
      #   councilR::theme_council() +
      #   theme(
      #     panel.grid.minor = element_blank(),
      #     panel.grid.major = element_blank(),
      #     strip.placement = "outside",
      #     axis.title.y = element_text(angle=0,
      #                                 vjust = .5),
      #     plot.margin = margin(7,7,7,7),
      #     axis.line = element_line(),
      #     axis.ticks = element_line()
      #
      #   ) +
      #   scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(.0,0)) +
      #   scale_x_continuous(#breaks = as_labeller(pbipoc = seq(0, 1, .25), hhincome = seq(0, 250000, 75000)),
      #                        # as.vector(c(seq(0,1, .25), seq(0, 250000, 75000))),#(c(seq(0, 1, .25), seq(0, 250000, 75000))),#
      #                      # breaks = breaks_fun,
      #                      limits = c(0, NA),
      #                      expand = c(0,0)
      #                      # labels = labs_fun
      #                      ) +
      #   labs(x = "", y = "Tree canopy\n (%)") +
      #   facet_wrap(~names,
      #              scales = "free_x", nrow = 2, strip.position = "bottom",
      #              labeller = as_labeller(c(pbipoc = "Population identifying as\nperson of color (%)", hhincome = "Median household\nincome ($)"))
      #   )
      #

      fig_equity <-
        ggplot(aes(x = raw_value, y = canopy_percent), data = df) +
        geom_point(col = "grey40", alpha = .2, data = filter(df, is.na(flag)), na.rm = T) +
        geom_smooth( # method = "lm",
          # formula = "y ~ x",
          method = "gam", formula = y ~ s(x, bs = "cs"),
          fill = NA, col = councilR::colors$councilBlue, na.rm = T,
          data = filter(df, names != "pbipoc")
        ) +
        geom_smooth( method = "lm",
          formula = "y ~ x",
          fill = NA, col = councilR::colors$councilBlue, na.rm = T,
          data = filter(df, names == "pbipoc")
        ) +
        geom_point(fill = councilR::colors$cdGreen, 
                   # size = 4, 
                   # stroke = 1, 
                   stroke = if(selected_length() > 100) {.5} else {1},
                   size = if (selected_length() > 100) {2} else {4},
                   col = "black", 
                   pch = 21, 
                   data = filter(df, flag == "selected"), 
                   na.rm = T) +
        councilR::theme_council() +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          strip.placement = "outside",
          axis.title.y = element_text(
            angle = 0,
            vjust = .5
          ),
          plot.margin = margin(7, 7, 7, 7),
          axis.line = element_line(),
          axis.ticks = element_line(),
          axis.text.y = element_text(vjust = .5, hjust = 1),
          plot.caption = element_text(
            size = rel(1),
            colour = "grey30"
          )
        ) +
        scale_y_continuous(
          labels = scales::percent_format(accuracy = 1),
          expand = expansion(mult = c(0, .05)),
          breaks = c(0, .15, .30, .45, .60)
        ) +
        # scale_x_continuous(breaks = breaks_fun,
        #                    limits = c(0, NA),
        #                    labels = labs_fun
        #                    ) +
        scale_x_continuous(
          labels = scales::comma,
          expand = expansion(mult = c(0, .1))
        ) +
        labs(
          x = "", y = "Tree\ncanopy\n (%)",
          caption = # expression(italic(
          "Source: Analysis of Sentinel-2 satellite imagery (2021), ACS 5-year \nestimates (2016-2020), and decennial census (2020)" # ))
        ) +
        facet_wrap(~names,
          scales = "free_x", nrow = 2, strip.position = "bottom",
          labeller = as_labeller(c(pbipoc = "Population identifying as\nperson of color (%)", hhincome = "Median household\nincome ($)"))
        )


      return(fig_equity)
    })

    # output$equity_plot <- renderPlot({
    #   req(TEST() != "")
    #   report_equity_plot()
    # })

    output$equity_plot <- renderImage(
      {
        req(TEST() != "")

        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- tempfile(fileext = ".png")

        # Generate the PNG
        png(outfile,
          width = 400 * 4,
          height = 450 * 4,
          res = 72 * 4
        )
        print(report_equity_plot())
        dev.off()

        # Return a list containing the filename
        list(
          src = outfile,
          contentType = "image/png",
          width = 400,
          height = 450,
          alt = "Figure showing the trends between tree canopy and median household income and the percent of population identifying as a person of color."
        )
      },
      deleteFile = TRUE
    )


    ndvilabs <- c(
      "<img src='inst/app/www/NDVI_.17.png' height='75' /><br>Low<br>green space",
      "<img src='inst/app/www/NDVI_.42.png' height='75' /><br>Moderate<br>green space",
      "<img src='inst/app/www/NDVI_.67.png' height='75' /><br>High<br>green space"
    )


    report_temp_plot <- reactive({
      req(TEST() != "")

      df <- param_equity() %>%
        select(flag, avg_temp, ndvi_land)

      plot <- ggplot(aes(x = ndvi_land, y = avg_temp), data = df) +
        geom_point(col = "grey40", alpha = .2, data = filter(df, is.na(flag)), na.rm = T) +
        geom_smooth(method = "lm", formula = "y ~ x + I(x^2)", fill = NA, col = councilR::colors$councilBlue) +
        geom_point(fill = councilR::colors$cdGreen, 
                   stroke = if(selected_length() > 100) {.5} else {1},
                   size = if (selected_length() > 100) {2} else {4},
                   
                   col = "black", pch = 21, data = filter(df, flag == "selected"), na.rm = T) +
        councilR::theme_council() +
        labs(
          x = "Amount of green space", y = "Summer\nland surface\ntemperature\n(°F)",
          caption = "\nSource: Analysis of Sentinel-2 satellite imagery (2021)\nand Landsat 8 satellite imagery (2016)"
        ) +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          strip.placement = "outside",
          axis.title.y = element_text(
            angle = 0,
            vjust = .5
          ),
          plot.margin = margin(7, 7, 14, 7),
          axis.line = element_line(),
          axis.ticks = element_line(),
          axis.text.y = element_text(vjust = .5, hjust = 1),
          plot.caption = element_text(
            size = rel(1),
            colour = "grey30"
          ),
          axis.text.x.bottom = ggtext::element_markdown(size = 15)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, .05))) +
        scale_x_continuous(
          name = NULL,
          breaks = c(.17, .42, .67),
          labels = ndvilabs,
          position = "bottom"
        )
      # return(plot)

      outfile <- tempfile(fileext = ".png")

      # Generate the PNG
      png(outfile,
        width = 400 * 2,
        height = 350 * 2,
        res = 72 * 2
      )
      print(plot)
      dev.off()

      #     fig <- list(src = outfile,
      #          contentType = 'image/png',
      #          width = 400,
      #          height = 300,
      #          alt = "Figure showing the trends between NDVI and land surface temperature.")
      #
      # return(fig)
      return(outfile)
    })

    output$temp_plot <- renderImage(
      {
        req(TEST() != "")
        list(
          src = report_temp_plot(),
          contentType = "image/png",
          width = 400,
          height = 350,
          alt = "Figure showing the trends between NDVI and land surface temperature."
        )
      },
      deleteFile = FALSE
    )

# 'Other' species make up a larger percent of the tree canopy today, but these species are mostly introduced species rather than a diverse assemblage of native species (as was the case before 1900). "

    param_reportname <- reactive({
      req(TEST() != "")
      paste0("GrowingShade_", param_area(), "_", Sys.Date(), ".html")
    })


    output$dl_report <- downloadHandler(
      filename = param_reportname, #paste0("GrowingShade_", param_area(), "_", Sys.Date(), ".html"), # ".docx"), # ".html"),
      content = function(file) {
        tempReport <- file.path(tempdir(), "downloadable_report.Rmd")
        tempCss <- file.path(tempdir(), "style.css")
        tempbdcn <- file.path(tempdir(), "helveticaneueltstd-bdcn-webfont.woff")
        tempcn <- file.path(tempdir(), "helveticaneueltstd-cn-webfont.woff")
        templt <- file.path(tempdir(), "helveticaneueltstd-lt-webfont.woff")
        tempmd <- file.path(tempdir(), "helveticaneueltstd-md-webfont.woff")
        tempmdcn <- file.path(tempdir(), "helveticaneueltstd-mdcn-webfont.woff")
        temproman <- file.path(tempdir(), "helveticaneueltstd-roman-webfont.woff")
        file.copy("downloadable_report.Rmd", tempReport, overwrite = TRUE)
        file.copy("inst/app/www/style.css", tempCss, overwrite = TRUE)
        file.copy("inst/app/www/helveticaneueltstd-bdcn-webfont.woff", tempbdcn, overwrite = TRUE)
        file.copy("inst/app/www/helveticaneueltstd-cn-webfont.woff", tempcn, overwrite = TRUE)
        file.copy("inst/app/www/helveticaneueltstd-lt-webfont.woff", templt, overwrite = TRUE)
        file.copy("inst/app/www/helveticaneueltstd-md-webfont.woff", tempmd, overwrite = TRUE)
        file.copy("inst/app/www/helveticaneueltstd-mdcn-webfont.woff", tempmdcn, overwrite = TRUE)
        file.copy("inst/app/www/helveticaneueltstd-roman-webfont.woff", temproman, overwrite = TRUE)

        imgOne <- file.path(tempdir(), "test.png")
        file.copy(report_temp_plot(), imgOne, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(
          param_geo = geo_selections$selected_geo,
          param_area = if (geo_selections$selected_geo == "blockgroups") {param_areasummary()$fancyname} else {param_area()},
          param_equitypara = tree_text(),
          param_treeplot = report_tree_plot(),
          param_ranktext = rank_text(),
          param_rankplot = report_rank_plot(),
          param_prioritytable = report_priority_table(),
          param_equitytext = equity_text(),
          param_equityplot = report_equity_plot(),
          param_tempplot = imgOne, # report_temp_plot(),
          # param_otherparea = report_other_para(),

          para_heattext = heat_text()
        )
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        # testcss <- file.path("style.css")
        rmarkdown::render(tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv()),
          # output_format = "pdf_document", #"html_document",
          output_format = "html_document",
          # output_format = "word_document", #word/pdf not yet accessible:https://www.rstudio.com/blog/knitr-fig-alt/
          output_options = list(
            html_preview = FALSE,
            toc = TRUE,
            # theme = "cosmo",
            toc_depth = 2,
            fig_caption = TRUE,
            # css = testcss
            css = tempCss
          )
        )
      }
    )



    output$dl_data <- downloadHandler(
      filename = function() {
        paste0("GrowingShade_", param_area(), "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(
          list(
            # "Metadata" = tibble(
            #   Metadata =
            #     c("Please use caution if using Excel formatting. You may need to divide cells by 100 for Excel to recognize percents correctly.",
            #       "This data is obviously not finished. If you are seeing this warning, please do not use!",
            #       "",
            #       "The interactive tool can be accessed at <https://metrotransitmn.shinyapps.io/growing-shade/>.")
            #   ),
            "Metadata" = metadata %>%
              filter(!is.na(name)) %>%
              mutate(nicer_interp = case_when(
                nicer_interp != "" ~ nicer_interp,
                niceinterp == "Lower" ~ "Lower values = higher priority",
                niceinterp == "Higher" ~ "Higher values = higher priority"
              )) %>%
              select(variable, name, nicer_interp, MEANRAW, climate_change, environmental_justice, public_health, conservation, n) %>%
              rename(
                `Variable` = variable,
                `Variable description` = name,
                `Value interpretation` = nicer_interp,
                `Region average` = MEANRAW,
                `Climate Change variable` = climate_change,
                `Environmental Justice variable` = environmental_justice,
                `Public Health variable` = public_health,
                `Conservation variable` = conservation,
                `Number of block groups with data` = n
              ),
            "Selected Area" = 
              (param_selectedblockgroupvalues() %>%
              select(
                GEO_NAME, jurisdiction, canopy_percent, MEAN,
                "Public health", Conservation, "Environmental justice", "Climate change"
              ) %>%
              rename(
                GEO_ID = GEO_NAME,
                `Selected priority score` = MEAN,
                `Climate change priority score` = `Climate change`,
                `Conservation priority score` = `Conservation`,
                `Environmental justice priority score` = `Environmental justice`,
                `Public health priority score` = `Public health`,
                `Percent tree cover` = canopy_percent
              ) %>%
              left_join(bg_growingshade_main %>%
                          select(bg_string, variable, raw_value) %>%
                          pivot_wider(names_from = variable, values_from = raw_value) %>%
                          rename(GEO_ID = bg_string), by = c("GEO_ID"))) 
               ,
            "Entire Region" = bg_growingshade_main %>%
              select(bg_string, variable, raw_value) %>%
              pivot_wider(names_from = variable, values_from = raw_value) %>%
              rename(GEO_ID = bg_string)
          ),
          path = file
        )
      }
    )

    
    output$shapefile_dl <- downloadHandler(

      filename <- function() {
        paste0("GrowingShade_", param_area(), "_", Sys.Date(), ".zip")
      },
      content = function(file) {
        withProgress(message = "Exporting Data", {
          
          incProgress(0.5)
          tmp.path <- dirname(file)
          
          name.base <- file.path(tmp.path, "GrowingShade")
          name.glob <- paste0(name.base, ".*")
          name.shp  <- paste0(name.base, ".shp")
          name.zip  <- paste0(name.base, ".zip")
          
          if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
          sf::st_write((param_selectedblockgroupvalues() %>%
                          select(
                            GEO_NAME, jurisdiction, canopy_percent, MEAN,
                            "Public health", Conservation, "Environmental justice", "Climate change"
                          ) %>%
                          rename(
                            GEO_ID = GEO_NAME,
                            `Selected priority score` = MEAN,
                            `Climate change priority score` = `Climate change`,
                            `Conservation priority score` = `Conservation`,
                            `Environmental justice priority score` = `Environmental justice`,
                            `Public health priority score` = `Public health`,
                            `Percent tree cover` = canopy_percent
                          ) %>%
                          left_join(bg_growingshade_main %>%
                                      select(bg_string, variable, raw_value) %>%
                                      pivot_wider(names_from = variable, values_from = raw_value) %>% select(-inverse_ndvi_uncultivated, -inverse_ndvi_land) %>%
                                      
                                      rename(GEO_ID = bg_string), by = c("GEO_ID"))),
                       dsn = name.shp, ## layer = "shpExport",
                       driver = "ESRI Shapefile", quiet = TRUE)
          
          zip::zipr(zipfile = name.zip, files = Sys.glob(name.glob))
          req(file.copy(name.zip, file))
          
          if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
          
          incProgress(0.5)
        })
      }  
    )

    ####### put things into reactive uis ----------



    # output$get_tree_plot <- renderUI({
    #   req(TEST() != "")
    #   plotOutput(ns("tree_plot"), "200px", width = "100%") %>%
    #     shinyhelper::helper(type = "markdown", content = "LineplotHelp", size = "m")
    # })

    # output$get_rank_plot <- renderUI({
    #   req(TEST() != "")
    #   plotOutput(ns("rank_plot"), "300px", width = "100%") %>%
    #     shinyhelper::helper(type = "markdown", content = "RankHelp", size = "m")
    # })


    # output$get_equity_plot <- renderUI({
    #   req(TEST() != "")
    #   plotOutput(ns("equity_plot"), height = "400px", width = "80%")
    # })

    # output$get_temp_plot <- renderUI({
    #   req(TEST() != "")
    #   plotOutput(ns("temp_plot"), "200px", width = "80%")
    # })


    # output$get_other_plot <- renderUI({
    #   req(TEST() != "")
    #   plotOutput(ns("other_plot"), "300px", width = "80%")
    # })
    
    output$treecanopy_box <- renderUI({
      req(TEST() != "")
      
      shinydashboard::box(
        title = ("Tree canopy"),
        width = 12, collapsed = shinybrowser::get_device() == "Mobile",
        status = "danger", solidHeader = F, collapsible = TRUE,
        (tree_text()),
        fluidRow(
          align = "center",
          if(shinybrowser::get_device() == "Mobile") {
            renderPlot(report_tree_plot())
             #plotOutput(ns("tree_plot"), "200px", width = "100%") #renderPlot, plotOutput
          } else {imageOutput(ns("tree_plot"), height = "100%", width = "100%")}
        ))
    })
    
    output$priority_box <- renderUI({
      req(TEST() != "")
      
      shinydashboard::box(
        title = "Prioritization",
        width = 12, collapsed = shinybrowser::get_device() == "Mobile",
        status = "danger", solidHeader = F, collapsible = TRUE,
        rank_text(),
        fluidRow(
          align = "center",
          if(shinybrowser::get_device() == "Mobile") {
            renderPlot(report_rank_plot())
            } else {
              imageOutput(ns("rank_plot"), height = "100%", width = "100%") }
        ),
        br(),
        tableOutput(ns("priority_table"))
      )
    })
    
    output$disparity_box <- renderUI({
      req(TEST() != "")

      shinydashboard::box(
        title = "Race & income disparities",
        width = 12, collapsed = shinybrowser::get_device() == "Mobile",
        status = "danger", solidHeader = F, collapsible = TRUE,
        equity_text(), #uiOutput(ns("equity_para")),
        # uiOutput(ns("get_equity_plot")),
        fluidRow(
          align = "center",
          if(shinybrowser::get_device() == "Mobile") {
            renderPlot(report_equity_plot())
          } else {
            imageOutput(ns("equity_plot"), height = "100%", width = "100%")}
        )
      )
    })
    
    output$temp_box <- renderUI({
      req(TEST() != "")
      
      shinydashboard::box(
        title = "Temperature",
        width = 12, collapsed = shinybrowser::get_device() == "Mobile",
        status = "danger", solidHeader = F, collapsible = TRUE,
        heat_text(), #uiOutput(ns("heat_para")),
        fluidRow(
          align = "center",
          # if(shinybrowser::get_device() == "Mobile") {
            # renderPlot(report_temp_plot())
          # } else {
            imageOutput(ns("temp_plot"), height = "100%", width = "100%")
            # }
        )
        # uiOutput(ns("get_temp_plot"))
      )
    })
    
    output$download_box <- renderUI({
      req(TEST() != "")
      
      shinydashboard::box(
        title = "Download data",
        width = 12, collapsed = shinybrowser::get_device() == "Mobile",
        status = "danger", solidHeader = F, collapsible = TRUE,
        HTML("<section class='d-none d-lg-block'>
             Use the buttons below to download a version of this report which can be printed or shared. 
             The raw data may also be downloaded as an excel or shapefile.<br></section>"),# uiOutput(ns("download_para")),
        HTML("<section class='d-block d-lg-none'>
             Download a complete version of this report. 
             Use a desktop computer to download raw data or shapefiles.<br></section>"),# uiOutput(ns("download_para")),
        fluidRow(
          column(width = 4, downloadButton(ns("dl_report"), label = "Text report")), #uiOutput(ns("get_the_report"))),
          column(class='d-none d-lg-block', width = 4, downloadButton(ns("dl_data"), label = "Raw data")), #uiOutput(ns("get_the_data"))),
          column(class='d-none d-lg-block', width = 4, downloadButton(ns("shapefile_dl"), label = "Shapefile")) #uiOutput(ns("get_shape_data")))
        )
      )
    })


    # output$get_the_report <- renderUI({
    #   req(TEST() != "")
    #   downloadButton(ns("dl_report"), label = "Text report")
    # })


    # output$get_the_data <- renderUI({
    #   req(TEST() != "")
    #   downloadButton(ns("dl_data"), label = "Raw data")
    # })

    
    
    # output$get_shape_data <- renderUI({
    #   req(TEST() != "")
    #   downloadButton(ns("shapefile_dl"), label = "Shapefile")
    # })
    


    # ######## make figs expand on click
    # shinyjs::onclick(('tree_plot'), #print("test"),
    #                  showModal(modalDialog(
    #   # title = "Tree canopy",
    #   # "Here is some information",
    #   # renderDataTable(data)
    #   renderPlot(report_tree_plot(), height = 400, width = 600),
    #   footer = modalButton("Close"),
    #   size = "l"
    #   # imageOutput(tree_plot())
    # )))
  })
}

## To be copied in the UI
# mod_report_ui("report_ui_1")

## To be copied in the server
# mod_report_server("report_ui_1")
