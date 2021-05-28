#' intro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_intro_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::div(
      id = "intro",
      includeMarkdown(system.file("app/www/intro.md", package = "eva.app"))
    )
  )
}

#' intro Server Function
#'
#' @noRd 
mod_intro_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_intro_ui("intro_ui_1")
    
## To be copied in the server
# callModule(mod_intro_server, "intro_ui_1")
 
