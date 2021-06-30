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

    shiny::div(
      id = "methods",
      includeMarkdown(system.file("app/www/methods.md", package = "planting.shade"))
    ),
    
    
    br(),
    # htmlOutput("frame"
    fluidRow(column(width = 12, align = "center",
                    tags$iframe(style="height:600px; width:70%; scrolling=yes", #style="height:600px; width:50%", 
                src="www/notes.pdf#zoom=110"))
    # div(style="display: inline-block;", embed(src="www/notes.pdf", height="100%")
    # shiny::div(
    #   id = "notes",
    #   includeMarkdown(system.file("app/www/notes.pdf", package = "planting.shade"))
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
 
