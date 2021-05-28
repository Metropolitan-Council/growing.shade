#' next UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_next_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::div(
      id = "next",
      includeMarkdown(system.file("app/www/next.md", package = "planting.shade"))
    )
  )
}

#' next Server Function
#'
#' @noRd 
mod_next_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_intro_ui("next_ui_1")
    
## To be copied in the server
# callModule(mod_next_server, "next_ui_1")
 
