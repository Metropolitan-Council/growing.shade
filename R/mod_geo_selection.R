#' geo_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import waiter
mod_geo_selection_ui <- function(id){
  ns <- NS(id)
  tagList(
    # waiter::useWaitress(),
    
    
    (radioButtons(
      ns("geo"),
      # h4("Report area"),
      label = HTML("<h3>Custom report</h3><p><section style='font-weight: normal;' >Make a selection to create a custom report which will generate below. <strong>Scoll down to keep reading.</strong></section></p>"),
      choices = c(
        "A Census tract (selected from the map)" = "tracts",
        "Cities and townships (select from dropdown below)" = "ctus",
        "Neighborhoods (select from dropdown which will appear below)" = "nhood"
      ), #multiple = F,
      selected = "tracts"
    )),
    
    # uiOutput(ns("geodropdowns"))
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
    # waitress <- Waitress$
    #   new(theme = "overlay-percent")$
    #   start() # start
    # 
    # for(i in 1:10){
    #   waitress$inc(10) # increase by 10%
    #   Sys.sleep(.3)
    # }
    # 
    # # hide when it's done
    # waitress$close() 
    
    # waitress <- Waitress$new(ns("#geodropdowns"))
    
    
    
    # output$geodropdowns <- renderUI({
    #   ns <- session$ns
    #   tagList(
    # 
    #     conditionalPanel(
    #     ns = ns,
    #     condition = "input.geo == 'ctus'",
    #     
    #     shinyWidgets::pickerInput(ns("cityInput"), 
    #                               label = shiny::HTML(paste0("<h4><span style='font-size:14pt'>City or township</span></h4>")),
    #                               choices = ctu_list$GEO_NAME,
    #                               options = list(title = "Pick a city or township", size = 20),
    #                               multiple = F
    #     )),
    #     
    #     conditionalPanel(
    #       ns = ns,
    #       condition = "input.geo == 'nhood'",
    #       
    #       shinyWidgets::pickerInput(ns("nhoodInput"), 
    #                                 label = shiny::HTML(paste0("<h4><span style='font-size:14pt'>Pick a neighborhood</span></h4>")),
    #                                 choices = list(
    #                                   Minneapolis = nhood_list$GEO_NAME[nhood_list$city=="Minneapolis"],
    #                                   `St. Paul` = nhood_list$GEO_NAME[nhood_list$city=="St. Paul"]),
    #                                 options = list(title = "Pick a neighborhood in St. Paul or Minneapolis", size = 20),
    #                                 multiple = F
    #       ))
    #   )
    #   
    #   # waitress$close()
    # })
    
    
    input_values <- reactiveValues()
    observe({
    input_values$selected_geo <- input$geo
    input_values$selected_area <- if(input$geo == "ctus") {
      input$cityInput
    } else if (input$geo == "nhood") {
        input$nhoodInput} else {""}
    
  })
    return(input_values)
  })
}
    
## To be copied in the UI
# mod_geo_selection_ui("geo_selection_ui_1")
    
## To be copied in the server
# mod_geo_selection_server("geo_selection_ui_1")
