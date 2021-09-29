#' geo_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_geo_selection_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    HTML("<h3>Custom report</h3><p><section style='font-weight: normal;' >Make selections below to create a custom report. Scoll to see more. Download it all at the bottom. </section></p><br>"),
    
    fluidRow(radioButtons(
      ns("geo"),
      h4("Report area"),
      choices = c(
        # "Selected tract" = "ctus",
        "Cities and townships" = "ctus",
        "Neighborhoods" = "nhood"
      ),
      selected = "ctus"
    )),
    
    conditionalPanel(
      ns = ns,
      condition = "input.geo == 'ctus'",
      
      shinyWidgets::pickerInput(ns("cityInput"), 
                                label = shiny::HTML(paste0("<h4><span style='font-size:14pt'>City or township</span></h4>")),
                                choices = ctus,
                                options = list(title = "Pick a city or township", size = 20),
                                multiple = F
      )),
    
    conditionalPanel(
      ns = ns,
      condition = "input.geo == 'nhood'",
      
      shinyWidgets::pickerInput(ns("cityInput"), 
                                label = shiny::HTML(paste0("<h4><span style='font-size:14pt'>Pick a neighborhood</span></h4>")),
                                choices = list(
                                  Minneapolis = nhood$nhood[nhood$city=="Minneapolis"],
                                  `St. Paul` = nhood$nhood[nhood$city=="Minneapolis"]),
                                options = list(title = "Pick a neighborhood in St. Paul or Minneapolis", size = 20),
                                multiple = F
      ))
  )
}
    
#' geo_selection Server Functions
#'
#' @noRd 
mod_geo_selection_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_geo_selection_ui("geo_selection_ui_1")
    
## To be copied in the server
# mod_geo_selection_server("geo_selection_ui_1")
