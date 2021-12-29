#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    tags$html(lang = "en"),
    # shiny::includeHTML("inst/app/www/google-analytics.html"),
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # List the first level UI elements here

    navbarPage(
      title = div(
        img(src = "www/main-logo.png", height = "60px", alt = "MetCouncil logo")
      ),
      windowTitle = "Growing Shade Tool",
      id = "nav",
      collapsible = TRUE,
      position = "fixed-top",
      header = tags$style(
        ".navbar-right {
                       float: right !important;
                       }",
        "body {padding-top: 75px;}"
      ),

      # tabPanel(
      #   "HOME",
      #   br(),
      #   br(),
      #   mod_intro_ui("intro_ui_1"),
      #   br()
      # ),

      tabPanel(
        "HOME",
        # id = "B",
        br(), br(),
        fluidRow((mod_storymap_ui("storymap_ui_1")))
      ),
      tabPanel(
        "Mapping tool",
        # id = "demo",
        div(
          style = "width:100% !important;
                    margin-left:0  !important; margin-top:30px  !important;
                    max-width: 4000px !important; min-width:100% !important",
          sidebarLayout(
            sidebarPanel(
              width = 6,
              style = "height: 90vh; overflow-y: auto;",

              # width = 2,
              h2("Welcome to the Growing Shade mapping tool"),
              br(),
              p(
                "Please refer to the ", a("text user guide",
                  href = "www/Growing Shade User Guide (November 2021).pdf",
                  .noWS = "outside",
                  target = "_blank"
                ),
                " or the ", a("video user guide",
                  href = "https://www.youtube.com/watch?v=R3Qbhaq4gWs",
                  .noWS = "outside",
                  target = "_blank"
                ), " for help. Customize and create reports using the options below. Zoom in, or turn on the tree layer, to explore the tree canopy in year 2020."
              ),
              # br(),
              # p("Fellow residents share the importance of greening within our region, while also detailing various approaches to achieve shared outcomes in the NARRATIVE tab. There is also more information about why we built this tool."),
              # # actionLink("link_to_tabpanel_b", "NARRATIVE tab"),
              # # p(" (link). The NARRATIVE tab also gives more information about why we built this tool. Please visit it!"),

              # a("Sinha 2021",
              #   href = ns("#B"), #"#tab-6122-2",
              #   .noWS = "outside"),
              # hr(style="margin-top: 2px; margin-bottom: 2px "),
              hr(),
              mod_map_selections_ui("map_selections_ui_1"),
              # HTML('<hr style="border-top: black;border-top-style: solid;border-right-width: 5px;">'),
              hr(style = "margin-top: 2px; margin-bottom: 2px "),

              # br(),
              mod_geo_selection_ui("geo_selection_ui_1"),
              # hr(style="margin-top: 2px; margin-bottom: 2px "),
              br(),
              mod_report_ui("report_ui_1") # ,
              # mod_plot_tract_ui("plot_tract_ui_1")
            ),
            mainPanel(
              width = 6,
              # div(class="outer3",
              div(
                style = "top:25em !important;", # style = 'width:100% !important; top:25em !important; ',
                mod_map_overview_ui("map_overview_ui_1")
              ),
            )
          )
        )
      ),
      tabPanel(
        "other Resources",
        mod_other_resources_ui("other_resources_ui_1") # mod_notes_ui("notes_ui_1")
      ),
      tabPanel(
        "methods",
        mod_methods_ui("methods_ui_1")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    shiny::includeHTML("inst/app/www/google-analytics.html"),
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Growing Shade Tool"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
