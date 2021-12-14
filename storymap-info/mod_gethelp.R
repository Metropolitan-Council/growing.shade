#' gethelp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_gethelp_ui <- function(id){
  ns <- NS(id)
  tagList(
    absolutePanel(
      id = "controls",
      class = "panel panel-default", 
      fixed = TRUE,
      draggable = TRUE, top = "13%", left = "auto", right = "5%", bottom = "auto",
      style = "padding: 7px",
      width = "auto", height = "auto",
      HTML('<button data-toggle="collapse" data-target="#helper">Get help</button>'),
      tags$div(id = 'helper',  class="collapse",
               
               HTML('Click on any area for more information.<br>
               Then you may create a report.
               <br><br>Read the <a href ="www/Growing Shade User Guide (August 2021).pdf" target="_blank">user guide</a> for more information.<br><br> 
                    We should also make a <a href ="https://youtu.be/gUBI4CIJNLQ" target="_blank">video user guide</a>.') 
               ))
  )
}
    
#' gethelp Server Functions
#'
#' @noRd 
mod_gethelp_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_gethelp_ui("gethelp_ui_1")
    
## To be copied in the server
# mod_gethelp_server("gethelp_ui_1")
