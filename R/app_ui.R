#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  
  navbar_js <- "@media (max-width: 840px) {
    .navbar-header {
        float: none;
    }
    .navbar-left,.navbar-right {
        float: none !important;
    }
    .navbar-toggle {
        display: block;
    }
    .navbar-collapse {
        border-top: 1px solid transparent;
        box-shadow: inset 0 1px 0 rgba(255,255,255,0.1);
    }
    .navbar-fixed-top {
        top: 0;
        border-width: 0 0 1px;
    }
    .navbar-collapse.collapse {
        display: none!important;
    }
    .navbar-nav {
        float: none!important;
        margin-top: 7.5px;
    }
    .navbar-nav>li {
        float: none;
    }
    .navbar-nav>li>a {
        padding-top: 10px;
        padding-bottom: 10px;
    }
    .collapse.in{
        display:block !important;
    }
}"
  
  
  tagList(
    tags$html(lang = "en"),
    tags$head(tags$style(HTML(navbar_js))),
    # shiny::includeHTML("inst/app/www/google-analytics.html"),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinydisconnect::disconnectMessage(
      text = HTML("Your session timed out. Please refresh the application."),
      refresh = "Refresh now",
      top = "center"
    ),


    # List the first level UI elements here
    # tags$head(img(src = "www/main-logo.png", height = "60px", alt = "MetCouncil logo")), #,'.navbar-brand{display:none;}')),
    navbarPage(
      title = div(style = "align:center",
        # img(src = "www/main-logo.png", height = "60px", alt = "MetCouncil logo"),
        # img(src = "www/main-logo.png", alt = "Met Council logo",
        #     style="margin-top: -30px; padding-left:0px",
        #     height = 60),
        # img(src = "www/Tree Trust Logo Color w Transparent Background (Avatar).png", alt = "Tree Trust logo",
        #     style="margin-top: -25px;",
        #     height = 120
        #     )
        
        a(href = "https://metrocouncil.org/", target = "_blank", 
          img(src = "www/main-logo.png", alt = "Met Council logo",
            # style="margin-top: -30px; padding-left:0px",
            height = 60)),
        a(href = "https://treetrust.org/non-profit/", target = "_blank", 
          img(src = "www/Tree Trust Logo Color w Transparent Background (Avatar).png", alt = "Tree Trust logo",
            # style="margin-top: -25px;",
            height = 60
        ))
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
      tabPanel(
        "HOME",
        # id = "B",
        br(), # br(),
        fluidRow((mod_storymap_ui("storymap_ui_1")))
      ),
      tabPanel(
        "Mapping tool",
        tags$footer(
          HTML('Source: <a href = "https://metrotransitmn.shinyapps.io/growing-shade/" target = "_blank">Growing Shade Project</a>. Last updated on 2022-01-20. '),
          align = "right",
          style = "
              position:absolute;
              bottom:1em;
              right:0;
              width:50%;
              height:20px;   /* Height of the footer */
              color: black;
              padding: 0px;
              background-color: transparent;
              z-index: 1000;"
        ),
        # id = "demo",
        div(
          style = "width:100% !important;
                    margin-left:0  !important; margin-top:30px  !important;
                    max-width: 4000px !important; min-width:100% !important",
          sidebarLayout(
            sidebarPanel(
              # waiter::useWaitress(),
              width = 6,
              style = "height: 90vh; overflow-y: auto;",

              # width = 2,
              HTML("<h1><section style='font-size: 22pt;'>Welcome to the Growing Shade mapping tool</h1></section>"),
              # h1("Welcome to the Growing Shade mapping tool"),
              br(),
              p(
                "Please refer to the ", a("text user guide",
                  href = "www/Growing Shade User Guide (January 2022).pdf",
                  .noWS = "outside",
                  target = "_blank"
                ),
                " or the ", a("video user guide",
                  href = "https://www.youtube.com/watch?v=R3Qbhaq4gWs",
                  .noWS = "outside",
                  target = "_blank"
                ), " for help. Customize and create reports using the options below. Zoom in, or turn on the tree layer, to explore the tree canopy in year 2021."
              ),
              hr(style = "margin-top: 2px; margin-bottom: 2px "),
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
              fluidRow(div(
                style = "top:25em !important;", # style = 'width:100% !important; top:25em !important; ',
                mod_map_overview_ui("map_overview_ui_1")
              )),
            )
          )
        )
      ),
      tabPanel(
        "Resources",
        mod_other_resources_ui("other_resources_ui_1")
      ),
      tabPanel(
        "FAQ",
        mod_faq_ui("faq_ui_1")
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
