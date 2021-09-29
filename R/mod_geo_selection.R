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
    
    (radioButtons(
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
                                choices = ctu_list$GEO_NAME,
                                options = list(title = "Pick a city or township", size = 20),
                                multiple = F
      )),
    
    conditionalPanel(
      ns = ns,
      condition = "input.geo == 'nhood'",
      
      shinyWidgets::pickerInput(ns("nhoodInput"), 
                                label = shiny::HTML(paste0("<h4><span style='font-size:14pt'>Pick a neighborhood</span></h4>")),
                                choices = list(
                                  Minneapolis = nhood_list$GEO_NAME[nhood_list$city=="Minneapolis"],
                                  `St. Paul` = nhood_list$GEO_NAME[nhood_list$city=="St. Paul"]),
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
 
    input_values <- reactiveValues()
    observe({
    input_values$selected_geo <- input$geo
    input_values$selected_area <- if(input$geo == "ctus") {input$cityInput} else {input$nhoodInput}
    
  })
    return(input_values)
  })
}
    
## To be copied in the UI
# mod_geo_selection_ui("geo_selection_ui_1")
    
## To be copied in the server
# mod_geo_selection_server("geo_selection_ui_1")
