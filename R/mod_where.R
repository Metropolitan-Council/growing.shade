#' where UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_where_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::div(
      id = "where",
      includeMarkdown(system.file("app/www/where.md", package = "eva.app"))
    )
  )
}

#' where Server Function
#'
#' @noRd 
mod_where_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_intro_ui("where_ui_1")
    
## To be copied in the server
# callModule(mod_where_server, "where_ui_1")
 
