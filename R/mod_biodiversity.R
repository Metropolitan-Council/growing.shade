#' biodiversity UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_biodiversity_ui <- function(id){
  ns <- NS(id)
  tagList(
    hr("What trees to plant and what outcomes to expect?"),
    p("Considering the 'what' to plant is the next logical step after identifying 'where' to plant trees. Reliable tree biodiversity data does not exist for the entire region, but we can incorporate what information we do have.")
 
  )
}
    
#' biodiversity Server Function
#'
#' @noRd 
mod_biodiversity_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_biodiversity_ui("biodiversity_ui_1")
    
## To be copied in the server
# callModule(mod_biodiversity_server, "biodiversity_ui_1")
 
