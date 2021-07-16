#' storymap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_storymap_ui <- function(id){
  ns <- NS(id)
  tagList(
    # uiOutput(ns("storymap"))
    # htmlOutput("storymap")
    # tags$iframe(src="https://storymaps.arcgis.com/stories/e61c8e0e54e24485b956601fdc80b63e", style='width:100%; margin:0;padding:0;height:80vh;border:none')#,
    
    htmlOutput(ns("storymap"))
    
    
    
    
    # width:100%
    # HTML('<center>
    #      <section style="position:absolute; height:100%; width:100%; border:none;"> <iframe src="https://storymaps.arcgis.com/stories/e61c8e0e54e24485b956601fdc80b63e"></iframe></section></center>'),
    # 
    # br(),
    # 
    # p("We can just have a nice hyperlink to the storymap link"),
    # HTML('<center><iframe width="560" height="500" src="https://metrocouncil.maps.arcgis.com/apps/MapJournal/index.html?appid=7d9cdd3929e9439bb5b25aa1186d5783"></iframe></center>')
  )
}
    
#' storymap Server Functions
#'
#' @noRd 
mod_storymap_server <- function(id){
  moduleServer( id, function(input, output, session
                             ){
    ns <- session$ns
                   
    # output$storymap <- renderUI({
    #   tags$iframe(src="https://storymaps.arcgis.com/stories/e61c8e0e54e24485b956601fdc80b63e", style='width:100%; margin:0;padding:0;height:80vh;border:none')
    # })
    
    output$storymap <- renderUI({
      my_test <- tags$iframe(src="https://storymaps.arcgis.com/stories/e61c8e0e54e24485b956601fdc80b63e", 
                             style='width:100%; margin:0;padding:0;height:80vh;border:none')
      my_test
    })
 
  })
}
    
## To be copied in the UI
# mod_storymap_ui("storymap_ui_1")
    
## To be copied in the server
# mod_storymap_server("storymap_ui_1")
