#' notes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_notes_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),br(),
    h1("Methods"),
    p("Priority variables with the exclusion of 'average greenness' were sourced from the 'Equity Considerations Dataset' published by the Met Council. Average greenness was calculated from Sentinel-2 satellite data processed on Google Earth Engine."),
    br(),
    p("Priority variables were scaled and standardized in order to create a single, integrated priority value. Download the pdf below for more information on that."),
    br(),
    p("Detailed priority areas were obtained from Sentinel-2 data. Maximum NDVI during 2020 was calculated at a 10 meter x 10 meter resolution. Due to processing speeds on this app (and the level of detail i think people might be interested in...), NDVI values have been rounded to the nearest tenth decimal point, and only areas where the peak NDVI in 2020 was below 0.50 are shown."),
    br(),
    # htmlOutput("frame"
    fluidRow(column(width = 12, align = "center",
                    tags$iframe(style="height:600px; width:70%; scrolling=yes", #style="height:600px; width:50%", 
                src="www/notes.pdf#zoom=110"))
    # div(style="display: inline-block;", embed(src="www/notes.pdf", height="100%")
    # shiny::div(
    #   id = "notes",
    #   includeMarkdown(system.file("app/www/notes.pdf", package = "eva.app"))
    ),
    br(),br()
  )
}
    
#' notes Server Function
#'
#' @noRd 
mod_notes_server <- function(input, output, session){
  ns <- session$ns
  
  # output$frame <- renderUI({
  #   my_test <- tags$iframe(src="www/notes.pdf", style="height:800px; width:100%;scrolling=yes")
  #   print(my_test)
  #   my_test
  # })
 
}
    
## To be copied in the UI
# mod_notes_ui("notes_ui_1")
    
## To be copied in the server
# callModule(mod_notes_server, "notes_ui_1")
 
