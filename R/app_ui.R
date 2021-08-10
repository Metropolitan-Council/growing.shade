#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    tags$html(lang = "en"),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
 
    navbarPage(title = div(img(src = "www/main-logo.png", height = "60px", alt = "MetCouncil logo")),
      id = "nav",
                collapsible = TRUE,
                # inverse = TRUE,
                # windowTitle = "EVA",
                position = "fixed-top",
                header = tags$style(
                  ".navbar-right {
                       float: right !important;
                       }",
                  "body {padding-top: 75px;}"),
               
               tabPanel("HOME", 
                        br(), br(),
                        mod_home_ui("home_ui_1"), br(), br(),
                        mod_intro_ui("intro_ui_1"), br()),
      tabPanel("narrative",
               br(), br(),
               mod_storymap_ui("storymap_ui_1")),
      
      tabPanel("Use the tool",
               br(), br(), br(),
              
               fluidRow(mod_where_ui("where_ui_1")), 
               
               fluidRow(
                 mod_preset_selections_ui("preset_selections_ui_1"),
                 # h3("Step 1: Select priority variables"), p("Select a preset or 'custom' variables and click 'update map.' Resulting values for each tract ranges from 0-10, and represents an average of standardized and scaled raw values."), 
                 br(),
                 mod_map_selections_ui("map_selections_ui_1")), hr(),
               fluidRow(
                 column(width = 6, h3("Step 2: View region-wide priority areas"), p("Warm and bright (yellow, orange) values and high ranks correspond to ‘opportunity zones’ where new tree plantings could have disproportionately positive impacts (values closer to 10). Cool and dark colors (black, purple) correspond to lower opportunity areas (values closer to zero)."), br(), mod_map_overview_ui("map_overview_ui_1")),
                 column(width = 6, h3("Step 3: Detailed priority areas"), p("Click on any area in Step 2 to see more detailed spatial locations which are in need of greening and may be suitable for tree planting. Darker red colors (and NDVI values closer to zero) indicate greater need for greening. No commerical or industrial land uses are shown (but can change!)."), br(), mod_ndvi_map_ui("ndvi_map_ui_1"))),
               br(), br()),
               
               
      tabPanel("other",
                        
                        fluidRow(
                          HTML("<section style=' background-image:url(./www/3789.png); 
                         min-height: 800px; background-attachment: fixed; background-position: center; background-repeat: no-repeat; background-size: cover;'><br><br>
                                     <h1 class='parallax' style='color:#78A22F; background-color: #ffffff; opacity: 0.8; padding:0px'>Why grow shade?</h1><br>
                                     <p class='parallax_description'>Explore the science and stories behind tree planting efforts.</p>
                                     </section>
                                     ")
                        ), br(),
                        
                        
                        
                        HTML("Placeholder, text and images for the narrative will go here. <br><br>Benefits of planting trees, challenges, etc. For brainstorming purposes, here are two ideas for showing how people connect with trees (an interactive story map or an interactive story generator). Of course, there are many other ways to consider doing something like this."),
                        
                        # mod_video_ui("video_ui_1"),
                        
                        mod_stories_ui("stories_ui_1"), br(), br(), #hr(), br(),
                        
                        # mod_why_ui("why_ui_1"),
                        
                        mod_story_generator_ui("story_generator_ui_1"), br(), hr(), br(),
                        
                        br(),hr(),br(),
                        
                        # fluidRow(column(width = 12, mod_next_ui("next_ui_1"))), br()
                        # Highways, railways, airports, and agricultural fields are not elligible for greening. Showing tree locations instead would be faster
                        
                        
                        # sidebarPanel(width = 3, 
                        #              mod_map_selections_ui("map_selections_ui_1")),
                        # mainPanel(width = 9,
                        #           fluidRow(mod_map_overview_ui("map_overview_ui_1"))),
                        # hr(), br(), br(),
                        # h1("Tract-specific information"), br(),
                        # HTML("<p>Click on a specific tract in the map above in order to view how it compares to the average tract. The table shows raw values (units vary across variables).</p>"), br(),
                        # # fluidRow(column(width = 12,
                        # #                 mod_plot_tract_ui("plot_tract_ui_1"),
                        # #                 hr()
                        # #                 # mod_evabar_ui("evabar_ui_1")
                        # #                 )),
                        
                        # mod_biodiversity_ui("biodiversity_ui_1"),
                        # br(), hr(), br(),
                        
                        fluidRow(
                          HTML("<section style=' background-image:url(./www/2413.png); 
                         min-height: 800px; background-attachment: fixed; background-position: center; background-repeat: no-repeat; background-size: cover;'><br><br>
                                     <h1 class='parallax' style='color:#78A22F; background-color: #ffffff; opacity: 0.8; padding:0px'>What else to consider?</h1><br>
                                     <p class='parallax_description'>Information to inform which species may be suitable for planting and the potential economic value of taking action.</p>
                                     </section>
                                     ")
                        ), br(),
                        
                        mod_biodiversity_ui("biodiversity_ui_1"),
                        
                        fluidRow(
                          HTML("<section style=' background-image:url(./www/3900.png); 
                         min-height: 800px; background-attachment: fixed; background-position: center; background-repeat: no-repeat; background-size: cover;'><br><br>
                                     <h1 class='parallax' style='color:#78A22F; background-color: #ffffff; opacity: 0.8; padding:0px'>Beta version feedback?</h1><br>
                                     <p class='parallax_description'>Should we pursue this? and other thoughts.....</p>
                                     </section>
                                     ")
                        ), br(),
                        
                        fluidRow(
                                 column(width = 12,
                                        mod_next_ui("next_ui_1")
                                        # mod_table_ui("table_ui_1")
                                        ))
    
    ),
    
  tabPanel("Methods",
           mod_notes_ui("notes_ui_1")
    # navbarMenu("Notes",
               # tabPanel("Data Sources", HTML("<br><br><br>"), mod_notes_ui("notes_ui_1")),
               # "----",
               # "Future steps",
               # tabPanel("Example", HTML("<br><br><br>A place holder to show how we might want to add information.")),
               # tabPanel("Example2", HTML("<br><br><br>And more info could be added in a fashion similar to this."))
    )
    
    ))
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'planting.shade'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

