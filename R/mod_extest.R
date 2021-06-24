#' extest UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_extest_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' extest Server Function
#'
#' @noRd 
mod_extest_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_extest_ui("extest_ui_1")
    
## To be copied in the server
# callModule(mod_extest_server, "extest_ui_1")
 
