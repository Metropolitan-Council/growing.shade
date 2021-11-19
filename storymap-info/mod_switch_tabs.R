#' switch_tabs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_switch_tabs_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns('jumpToP2'), 'Jump to Second Tab')
  )
}
    
#' switch_tabs Server Functions
#'
#' @noRd 
mod_switch_tabs_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$jumpToP2, {
      updateTabsetPanel(session, "nav",
                        selected = ns("narrative"))
    })
    
    # observeEvent(input$switchbutton, {
    #   updateTabsetPanel(session = session,
    #                     inputId = "narrative",
    #                     selected = "narrative")
    # })
 
  })
}
    
## To be copied in the UI
# mod_switch_tabs_ui("switch_tabs_ui_1")
    
## To be copied in the server
# mod_switch_tabs_server("switch_tabs_ui_1")
