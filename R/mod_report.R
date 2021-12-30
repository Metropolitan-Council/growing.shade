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
    shinyWidgets::useShinydashboard(),
    fluidRow(
      column(width = 6, uiOutput(ns("get_the_report"))),
      column(width = 6, uiOutput(ns("get_the_data")))
    ),
    (uiOutput(ns("geoarea"))),
    br(),
    fluidRow(shinydashboard::box(
      title = ("Tree canopy: "),
      width = 12, collapsed = F,
      status = "danger", solidHeader = F, collapsible = TRUE,
      uiOutput(ns("tree_para")),
      uiOutput(ns("get_tree_plot"))
    )),
    fluidRow(shinydashboard::box(
      title = "Priortization: ",
      width = 12, collapsed = F,
      status = "danger", solidHeader = F, collapsible = TRUE,
      uiOutput(ns("rank_para")),
      uiOutput(ns("get_rank_plot")),
      br(),
      tableOutput(ns("priority_table"))
    )),
    fluidRow(shinydashboard::box(
      title = "Equity: ",
      width = 12, collapsed = F,
      status = "danger", solidHeader = F, collapsible = TRUE,
      uiOutput(ns("equity_para")),
      uiOutput(ns("get_equity_plot"))
    )),
    fluidRow(shinydashboard::box(
      title = "Threats: ",
      width = 12, collapsed = F,
      status = "danger", solidHeader = F, collapsible = TRUE,
      uiOutput(ns("other_para"))
    ))
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
                              map_util) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ####### things to export
    TEST <- reactive({
      TEST <- if (geo_selections$selected_geo == "ctus") {
        geo_selections$selected_area
      } else if (geo_selections$selected_geo == "nhood") {
        geo_selections$selected_area
      } else if (geo_selections$selected_geo == "tracts") {
        tract_selections$selected_tract
      }
      return(TEST)
    })

    param_area <- reactive({
      req(TEST() != "")
      output <- TEST()
      return(output)
    })

    param_fancytract <- reactive({
      req(geo_selections$selected_geo == "tracts")
      fancyname <-
        paste0(
          if (substr(param_area(), 3, 5) == "053") {
            "Hennepin County block group "
          } else if (substr(param_area(), 3, 5) == "003") {
            "Anoka County block group "
          } else if (substr(param_area(), 3, 5) == "019") {
            "Carver County block group "
          } else if (substr(param_area(), 3, 5) == "037") {
            "Dakota County block group "
          } else if (substr(param_area(), 3, 5) == "123") {
            "Ramsey County block group "
          } else if (substr(param_area(), 3, 5) == "139") {
            "Scott County block group "
          } else if (substr(param_area(), 3, 5) == "163") {
            "Washington County block group "
          },
          as.numeric(substr(param_area(), 6, 11)) / 100
        )
      return(fancyname)
    })


    # the min, max, ntracts, eab, treeacres, landacres, canopypercent, avgcanopy for the selected geography
    param_areasummary <- reactive({
      req(TEST() != "")
      output <- if (geo_selections$selected_geo == "ctus") {
        sf::st_drop_geometry(ctu_list[ctu_list$GEO_NAME == param_area(), ])
      } else if (geo_selections$selected_geo == "nhood") {
        sf::st_drop_geometry(nhood_list[nhood_list$GEO_NAME == param_area(), ])
      } else if (geo_selections$selected_geo == "tracts") {
        sf::st_drop_geometry(mn_tracts[mn_tracts$GEOID == param_area(), ])
      }
      return(output)
    })

    # the min/max/other data for all tracts within a given ctu/nhood/tract (n = 1 for tracts, n > 1 for most ctus/nhoods)
    param_selectedtractvalues <- reactive({
      req(TEST() != "")
      output <- filter(
        (map_util$map_data2),
        tract_string %in%
          if (geo_selections$selected_geo == "ctus") {
            c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == param_area(), ]$tract_id)
          } else if (geo_selections$selected_geo == "nhood") {
            c(nhood_crosswalk[nhood_crosswalk$GEO_NAME == param_area(), ]$tract_id)
          } else {
            c(param_area())
          }
      )
      return(output)
    })

    # all data with flag for selected areas
    param_dl_data <- reactive({
      req(TEST() != "")

      output <- bg_growingshade_main %>%
        mutate(flag = if_else(tract_string %in%
          if (geo_selections$selected_geo == "ctus") {
            c(ctu_crosswalk[ctu_crosswalk$GEO_NAME == param_area(), ]$tract_id)
          } else if (geo_selections$selected_geo == "nhood") {
            c(nhood_crosswalk[nhood_crosswalk$GEO_NAME == param_area(), ]$tract_id)
          } else if (geo_selections$selected_geo == "tracts") {
            c(param_area())
          },
        "selected", NA_character_
        ))
      return(output)
    })


    param_equity <- reactive({
      equityplot <- param_dl_data() %>%
        filter(variable %in% c("pbipoc", "canopy_percent", "mdhhincnow")) %>%
        select(tract_string, variable, raw_value, flag) %>%
        pivot_wider(names_from = variable, values_from = raw_value)
      return(equityplot)
    })


    output$geoarea <- renderUI({
      ns <- session$ns
      tagList(
        HTML(paste0(
          "<h2><section style='font-size:20pt'>Growing Shade report for ",
          if (geo_selections$selected_geo == "tracts") {
            param_fancytract()
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
          if (geo_selections$selected_geo == "tracts") {
            paste0(
              param_fancytract(), " has an existing tree canopy coverage of ", round(param_areasummary()$canopy_percent * 100, 2),
              "% in 2020. Compared to other block groups across the region, the tree canopy in ", param_fancytract(), " is ",
              if (param_areasummary()$canopy_percent > (param_areasummary()$avgcanopy + .02)) {
                "above"
              } else if (param_areasummary()$canopy_percent < (param_areasummary()$avgcanopy - .02)) {
                "below"
              } else {
                "about equal to"
              },
              " average (", round(param_areasummary()$avgcanopy * 100, 1), "%).<br><br> ",
              "The distribution of tree canopy coverage across all of the region's block groups is shown below; the selected block group is highlighted in green."
            )
          } else {
            paste0(
              param_area(),
              " has an existing tree canopy coverage of ", round(param_areasummary()$canopy_percent * 100, 1),
              "% in 2020. Compared to other ", if (geo_selections$selected_geo == "ctus") {
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
              param_areasummary()$ntracts,
              " Census block groups with tree canopy cover ranging from ",
              param_areasummary()$min,
              "% to ",
              param_areasummary()$max,
              "%. <br><br>The distribution of tree canopy is shown below. The selected area is highlighted in green. Census block groups have different areas and may overlap with other geographies, thus the exisiting tree canopy cover in the selected area may not be the mean of the block group canopy covers."
            )
          }
        )
      ))
    })

    output$tree_para <- renderUI({
      req(TEST() != "")
      HTML(paste0(tree_text(), " Read the methods in the 'other resources' tab to understand why our canopy cover numbers may differ from other tools."))
    })


    tree_report_plot <- reactive({
      req(TEST() != "")
      if (geo_selections$selected_geo != "tracts") {
        canopyplot <-
          (as_tibble(if (geo_selections$selected_geo == "ctus") {
            ctu_list
          } else {
            nhood_list
          }) %>%
            mutate(flag = if_else(GEO_NAME == param_area(), "selected", NA_character_)) %>%
            rename(tract_string = GEO_NAME) %>%
            select(tract_string, canopy_percent, flag) %>%
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

      if (geo_selections$selected_geo != "tracts") {
        plot <- ggplot() +
          geom_boxplot(
            data = canopyplot, aes(x = raw_value, y = type),
            outlier.shape = NA,
            na.rm = T
          ) +
          councilR::council_theme() +
          theme(
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.text.y = element_text(size = 12)
          ) +
          ggtitle(paste0(param_area(), " tree canopy")) +
          geom_point(
            size = 1.3, alpha = .3,
            position = position_jitter(seed = 1, width = 0, height = .3),
            col = "grey40",
            aes(x = raw_value, y = type),
            data = filter(canopyplot, is.na(flag)),
            na.rm = T
          ) +
          labs(y = "", x = "Tree canopy cover (%)") +
          scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
          geom_point(aes(x = raw_value, y = type),
            fill = councilR::colors$cdGreen,
            size = 4, col = "black", pch = 21,
            data = filter(canopyplot, flag == "selected", is.na(t2))
          ) +
          geom_jitter(aes(x = raw_value, y = type),
            position = position_jitter(seed = 1, width = 0, height = .3),
            fill = councilR::colors$cdGreen,
            size = 4, col = "black", pch = 21,
            data = filter(canopyplot, flag == "selected", t2 == "tracts"),
            na.rm = T
          )
      } else {
        plot <- ggplot() +
          ggdist::stat_halfeye(
            data = canopyplot, aes(x = raw_value, y = 1),
            adjust = .5, width = .6, .width = 0, justification = -.6,
            point_colour = NA,
            na.rm = T
          ) +
          geom_boxplot(
            data = canopyplot, aes(x = raw_value, y = 1),
            width = .75, outlier.shape = NA,
            na.rm = T
          ) +
          councilR::council_theme() +
          theme(
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.text.y = element_blank()
          ) +
          ggtitle(paste0(param_fancytract(), " tree canopy")) +
          geom_point(
            size = 1.3, alpha = .3,
            position = position_jitter(seed = 1, width = 0, height = .3),
            col = "grey40",
            aes(x = raw_value, y = 1),
            data = filter(canopyplot, is.na(flag)),
            na.rm = T
          ) +
          labs(y = "", x = "Tree canopy cover (%)") +
          scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
          geom_point(aes(x = raw_value, y = 1),
            fill = councilR::colors$cdGreen,
            size = 5, col = "black", pch = 21,
            data = filter(canopyplot, flag == "selected"),
            na.rm = T
          )
      }
      return(plot)
    })

    output$tree_plot <- renderPlot(
      {
        req(TEST() != "")
        tree_report_plot()
      } # , res = 150)
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
            paste0(
              param_fancytract(), " has a priority score of ",
              round((param_selectedtractvalues()$MEAN), 2),
              " (out of 10, where 10 indicates the highest priority) with a region-wide ranking of ",
              (param_selectedtractvalues()$RANK), " (out of 2085 total block groups across the region). A plot of the block group rankings for all presets is shown below. A table containing the raw values of the variables used in the selected preset (",
              tolower(map_selections$preset), ") is also shown below. In the table, the average values for the selected area are compared to the region-wide averages.<br><br>"
            )
          } else {
            paste0(
              "block groups within ",
              param_area(),
              " have overall priority scores ranging from ",
              round(min(param_selectedtractvalues()$MEAN), 2), " to ", round(max(param_selectedtractvalues()$MEAN), 2),
              " and a region-wide ranking from ",
              min(param_selectedtractvalues()$RANK), " to ", max(param_selectedtractvalues()$RANK), " (where a higher rank (closer to 1) indicates higher priorities). A plot of the block group rankings for all presets is shown below. A table containing the raw values of the variables used in the selected preset (",
              tolower(map_selections$preset), ") is also shown below. In the table, the average values for the selected area are compared to the region-wide averages.<br><br>"
            )
          }
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
      test2 <- if (map_selections$preset != "Custom") {
        tibble()
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
        st_drop_geometry() %>%
        select(`Public health`, Conservation, `Environmental justice`, `Climate change`, GEO_NAME) %>%
        pivot_longer(names_to = "priority", values_to = "rank", -GEO_NAME) %>%
        bind_rows(test2)

      plot <- # if (map_selections$priority_layer == "Off") {print("nothing to see here")
        # } else {
        ggplot() +
        scale_x_continuous(limits = c(1, 2085), labels = c(1, 500, 1000, 1500, 2085), breaks = c(1, 500, 1000, 1500, 2085)) +
        geom_errorbarh(data = test, aes(xmax = rank, xmin = rank, y = forcats::fct_rev(priority)), height = .5) +
        councilR::council_theme() +
        theme( # axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
        ) +
        geom_segment(aes(
          x = 1, xend = 2085, y = segment_line$y,
          yend = segment_line$y
        )) +
        labs(x = "Rank of aggregated priority score\n(out of 2085 block groups across the region)")
      # }
      return(plot)
    })

    output$rank_plot <- renderPlot({
      req(TEST() != "")
      report_rank_plot()
    })

    # priority section -----------

    report_priority_table <- reactive({
      req(TEST() != "")

      step1 <- param_dl_data() %>%
        filter(name %in%
          if (map_selections$preset == "Environmental justice") {
            metadata[metadata$ej == 1, ]$name
          } else if (map_selections$preset == "Climate change") {
            metadata[metadata$cc == 1, ]$name
          } else if (map_selections$preset == "Public health") {
            metadata[metadata$ph == 1, ]$name
          } else if (map_selections$preset == "Conservation") {
            metadata[metadata$cons == 1, ]$name
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
              metadata[metadata$ej == 1, ]$name
            } else if (map_selections$preset == "Climate change") {
              metadata[metadata$cc == 1, ]$name
            } else if (map_selections$preset == "Public health") {
              metadata[metadata$ph == 1, ]$name
            } else if (map_selections$preset == "Conservation") {
              metadata[metadata$cons == 1, ]$name
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


    output$equity_para <- renderUI({
      ns <- session$ns
      req(TEST() != "")
      para <- HTML(paste0(
        "Research shows that trees are unevenly distributed across communities. ",
        "Areas with a high percent of the population identifying as a person of color or low-income populations have less tree canopy. ",
        "In the plot below, ",
        if (geo_selections$selected_geo == "tracts") {
          paste0(param_fancytract(), " is ")
        } else {
          paste0(
            "block groups within ",
            param_area(),
            " are "
          )
        },
        "in green, and the regional trend is in blue."
      ))
      return(para)
    })

    report_equity_plot <- reactive({
      req(TEST() != "")
      # race_equity <- param_equity() %>%
      #   ggplot(aes(x = pbipoc, y = canopy_percent)) +
      #   geom_point(col = "grey40", alpha = .3, data = filter(param_equity(), is.na(flag)), na.rm = T) +
      #   geom_smooth(method = "lm", formula = 'y ~ x', fill = NA, col = councilR::colors$councilBlue, data = param_equity(), na.rm = T) +
      #   geom_point(fill = councilR::colors$cdGreen, size = 5, col = "black", pch = 21, data = filter(param_equity(), flag == "selected"), na.rm = T) +
      #   councilR::council_theme() +
      #   theme(panel.grid.minor = element_blank(),
      #         panel.grid.major = element_blank()) +
      #   scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      #   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      #   labs(x = "BIPOC population\n(%)", y = "Tree canopy\n (%)")
      #
      # inc_equity <- param_equity()%>%
      #   ggplot(aes(x = mdhhincnow/1000, y = (canopy_percent))) +
      #   geom_point(col = "grey40", alpha = .3, data = filter(param_equity(), is.na(flag)), na.rm = T) +
      #   geom_smooth(method = "lm",  formula = 'y ~ x', fill = NA, col = councilR::colors$councilBlue, na.rm = T) +
      #   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      #   scale_x_continuous(labels = scales::dollar_format(accuracy = 1)) +
      #   geom_point(fill = councilR::colors$cdGreen, size = 5, col = "black", pch = 21, data = filter(param_equity(), flag == "selected"), na.rm = T) +
      #   councilR::council_theme() +
      #   theme(panel.grid.minor = element_blank(),
      #         panel.grid.major = element_blank()) +
      #   labs(x = "Median household income\n($, thousands)", y = "Tree canopy\n (%)")# +
      #   # theme(axis.title.y = element_blank(),
      #   #       axis.text.y = element_blank())
      # fig_equity <- cowplot::plot_grid(race_equity, inc_equity, nrow = 2, labels = "AUTO")
      # return(fig_equity)

      df <- param_equity() %>%
        select(flag, canopy_percent, mdhhincnow, pbipoc) %>%
        pivot_longer(names_to = "names", values_to = "raw_value", -c(flag, canopy_percent)) %>%
        mutate(raw_value = if_else(names == "pbipoc", raw_value * 100, raw_value / 1000))

      fig_equity <-
        ggplot(aes(x = raw_value, y = canopy_percent), data = df) +
        geom_point(col = "grey40", alpha = .3, data = filter(df, is.na(flag)), na.rm = T) +
        geom_smooth(method = "lm", formula = "y ~ x", fill = NA, col = councilR::colors$councilBlue, na.rm = T) +
        geom_point(fill = councilR::colors$cdGreen, size = 5, col = "black", pch = 21, data = filter(df, flag == "selected"), na.rm = T) +
        councilR::council_theme() +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          strip.placement = "outside"
        ) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(x = "", y = "Tree canopy\n (%)") +
        facet_wrap(~names,
          scales = "free_x", nrow = 2, strip.position = "bottom",
          labeller = as_labeller(c(pbipoc = "Population identifying as\nperson of color (%)", mdhhincnow = "Median household income\n($, thousands)"))
        )

      return(fig_equity)
    })

    output$equity_plot <- renderPlot({
      req(TEST() != "")
      report_equity_plot()
    })


    report_other_para <- reactive({
      ns <- session$ns
      req(TEST() != "")
      tagList(
        HTML(paste0(
          # "The goal of this section is present information about biodiversity, management challenges, and other considerations for managing the tree canopy.<br><br>",
          "The Emerald ash borer (EAB) insect is a major threat to existing tree canopy. Data shows that EAB has infested ",
          param_areasummary()$EAB, " trees in ",
          if (geo_selections$selected_geo == "tracts") {
            param_fancytract()
          } else {
            param_area()
          }, " (",
          a("Minnesota DNR",
            href = "https://mnag.maps.arcgis.com/apps/webappviewer/index.html?id=63ebb977e2924d27b9ef0787ecedf6e9",
            .noWS = "outside",
            target = "_blank"
          ),
          "). Please note that these data are not necessarily intended to identify every ash tree (infested or not), however this information may still be useful.<br><br>",
          "Regional information about considerations related to climate change, the biodiversity of the existing tree canopy, and others are given under the 'other resources' tab at top.<br><br>"
          # "Low biodiversity is another threat to the tree canopy in the region. And knowing which species can adapt to a changing climate. Over the last 100 years, our region has seen a decline in oak trees, and an increase in ash, elm, and maple trees (<a href = 'https://gisdata.mn.gov/dataset/biota-original-pls-bearing-trees' target = '_blank'>Almendinger 1997</a>, <a href = 'https://www.nrs.fs.fed.us/data/urban/state/city/?city=6#ufore_data' target = '_blank'>Davey Resource Group 2004</a>). 'Other' species make up a larger percent of the tree canopy today, but these species are mostly introduced species rather than a diverse assemblage of native species (as was the case before 1900). "
        ))
      )
    })

    output$other_para <- renderUI({
      req(TEST() != "")
      report_other_para()
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
      filename = paste0("GrowingShade_", param_area(), "_", Sys.Date(), ".html"), # ".docx"), # ".html"),
      content = function(file) {
        tempReport <- file.path(tempdir(), "report_new.Rmd")
        tempCss <- file.path(tempdir(), "style.css")
        tempbdcn <- file.path(tempdir(), "helveticaneueltstd-bdcn-webfont.woff")
        tempcn <- file.path(tempdir(), "helveticaneueltstd-cn-webfont.woff")
        templt <- file.path(tempdir(), "helveticaneueltstd-lt-webfont.woff")
        tempmd <- file.path(tempdir(), "helveticaneueltstd-md-webfont.woff")
        tempmdcn <- file.path(tempdir(), "helveticaneueltstd-mdcn-webfont.woff")
        temproman <- file.path(tempdir(), "helveticaneueltstd-roman-webfont.woff")
        file.copy("report_new.Rmd", tempReport, overwrite = TRUE)
        file.copy("inst/app/www/style.css", tempCss, overwrite = TRUE)
        file.copy("inst/app/www/helveticaneueltstd-bdcn-webfont.woff", tempbdcn, overwrite = TRUE)
        file.copy("inst/app/www/helveticaneueltstd-cn-webfont.woff", tempcn, overwrite = TRUE)
        file.copy("inst/app/www/helveticaneueltstd-lt-webfont.woff", templt, overwrite = TRUE)
        file.copy("inst/app/www/helveticaneueltstd-md-webfont.woff", tempmd, overwrite = TRUE)
        file.copy("inst/app/www/helveticaneueltstd-mdcn-webfont.woff", tempmdcn, overwrite = TRUE)
        file.copy("inst/app/www/helveticaneueltstd-roman-webfont.woff", temproman, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(
          param_geo = geo_selections$selected_geo,
          param_area = param_area(),
          param_equitypara = tree_text(),
          param_treeplot = tree_report_plot(),
          param_ranktext = rank_text(),
          param_rankplot = report_rank_plot(),
          param_prioritytable = report_priority_table(),
          param_equityplot = report_equity_plot(),
          param_otherparea = report_other_para()
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
            toc_depth = 3,
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
            "Metadata" = tibble() %>%
              rbind(
                c("", "", ""),
                c("Please use caution if using Excel formatting. You may need to divide cells by 100 for Excel to recognize percents correctly.", "", ""),
                c("This data is obviously not finished. If you are seeing this warning, please do not use!", "", ""),
                c("The interactive tool can be accessed at <https://metrotransitmn.shinyapps.io/growing-shade/>.", "", "")
              ),
            "Selected Area" = param_selectedtractvalues(),
            "Region Averages" = metadata %>%
              filter(!is.na(name)) %>%
              mutate(nicer_interp = case_when(
                nicer_interp != "" ~ nicer_interp,
                niceinterp == "Lower" ~ "Lower values = higher priority",
                niceinterp == "Higher" ~ "Higher values = higher priority"
              )) %>%
              select(name, variable, nicer_interp, MEANRAW, cc, ej, ph, cons, n) %>%
              rename(
                `Value interpretation` = nicer_interp,
                `Block group average` = MEANRAW,
                `Climate Change variable` = cc,
                `Environmental Justice variable` = ej,
                `Public Health variable` = ph,
                `Conservation variable` = cons,
                `Number of block groups with data` = n
              )
          ),
          path = file
        )
      }
    )


    ####### put things into reactive uis ----------

    output$tree_title <- renderUI({
      req(TEST() != "")
      h4("Tree canopy: ")
    })

    output$priority_title <- renderUI({
      req(TEST() != "")
      h4("Priortization: ")
    })

    output$equity_title <- renderUI({
      req(TEST() != "")
      h4("Equity: ")
    })

    output$other_title <- renderUI({
      req(TEST() != "")
      h4("Threats: ")
    })


    output$get_tree_plot <- renderUI({
      req(TEST() != "")
      plotOutput(ns("tree_plot"), "200px", width = "100%") %>%
        shinyhelper::helper(type = "markdown", content = "LineplotHelp", size = "m")
    })

    output$get_rank_plot <- renderUI({
      req(TEST() != "")
      plotOutput(ns("rank_plot"), "300px", width = "100%") %>%
        shinyhelper::helper(type = "markdown", content = "RankHelp", size = "m")
    })


    output$get_equity_plot <- renderUI({
      req(TEST() != "")
      plotOutput(ns("equity_plot"), "400px", width = "80%")
    })


    output$get_other_plot <- renderUI({
      req(TEST() != "")
      plotOutput(ns("other_plot"), "300px", width = "80%")
    })


    output$get_the_report <- renderUI({
      req(TEST() != "")
      downloadButton(ns("dl_report"), label = "Comprehensive report")
    })


    output$get_the_data <- renderUI({
      req(TEST() != "")
      downloadButton(ns("dl_data"), label = "Raw data")
    })
  })
}

## To be copied in the UI
# mod_report_ui("report_ui_1")

## To be copied in the server
# mod_report_server("report_ui_1")
