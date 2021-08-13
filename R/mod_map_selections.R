#' map_selections UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_map_selections_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    wellPanel(
      id = "controls",
      radioButtons(ns("preset"), h3("Select preset"),
                  choices = c(
                    "Climate change",
                    "Conservation",
                    "Environmental justice",
                    "Public health",
                    "Custom"
                  ), 
                  selected = "Custom"
      ),
      
      conditionalPanel(
        ns = ns,
        condition = "input.preset == 'Custom'",
        
        shinyWidgets::pickerInput(ns("peopleInput"), 
                                  label = shiny::HTML(paste0("<h4><span style='font-size:14pt'>People & Equity</span></h4>")),
                                  choices = filter(metadata, type == "people") %>% .$name,
                                  options = list(`actions-box` = TRUE,
                                                 size = 20,
                                                 `selected-text-format` = "count > 1"),
                                  multiple = T,
                                  selected = NULL #filter(metadata, type == "people")[1, 2]
        )),
        conditionalPanel(
          ns = ns,
          condition = "input.preset == 'Custom'",
          
          shinyWidgets::pickerInput(ns("placeInput"), 
                                    label = shiny::HTML(paste0("<h4><span style='font-size:14pt'>Environment & Climate</span></h4>")),
                                    choices = filter(metadata, type == "environment") %>% .$name,
                                    options = list(`actions-box` = TRUE,
                                                   size = 20,
                                                   `selected-text-format` = "count > 1"),
                                    multiple = T,
                                    selected = NULL #filter(metadata, type == "environment")[1, 2]
          )),
          conditionalPanel(
            ns = ns,
            condition = "input.preset == 'Custom'",
            
            shinyWidgets::pickerInput(ns("treeInput"), 
                                      label = shiny::HTML(paste0("<h4><span style='font-size:14pt'>Existing tree canopy</span></h4>")),
                                      choices = filter(metadata, type == "tree") %>% .$name,
                                      options = list(`actions-box` = TRUE,
                                                     size = 20,
                                                     `selected-text-format` = "count > 1"),
                                      multiple = T,
                                      selected = filter(metadata, type == "tree")[1, 2]
            )
      ))
    
    
    ##############
    # # #old way to show dropdowns for eveyrthing
    #   uiOutput(ns("peopleInput")),
    #   uiOutput(ns("placeInput")),
    #   uiOutput(ns("treeInput"))
    
    # # maybe want to do a weighting option???,
    # radioButtons(ns("weight"),
    #              label = shiny::HTML("<h4>Variable weights</h4>", "<p style='font-size:16px'>Choose how tract scores are calculated.</p>"),
    #              choices = c("Weight all variables equally", "Weight all categories equally"),
    #              selected = "Weight all variables equally",
    #              inline = T), br(),

  )
}
    
#' map_selections Server Function
#'
#' @noRd 
mod_map_selections_server <- function(input, output, session,
                                      preset_selections,
                                      current_tab){
  ns <- session$ns
  
  input_values <- reactiveValues() # start with an empty reactiveValues object.
  observe({ input_values$allInputs <-  as_tibble(input$peopleInput) %>%
        rbind(as_tibble(input$placeInput)) %>%
        rbind(as_tibble(input$treeInput))
  
  input_values$preset <- input$preset
  })
  return(input_values)

  
  ############
  # # #old way to show dropdowns for everything
  # output$peopleInput <- renderUI({
  #   ns <- session$ns
  #   tagList(  
  #     a <- shinyWidgets::pickerInput(ns("peopleInput"), 
  #                               label = shiny::HTML(paste0("<h4><span style='font-size:14pt'>People & Equity</span></h4>")),
  #                               choices = filter(metadata, type == "people") %>% .$name,
  #                               options = list(`actions-box` = TRUE,
  #                                              size = 20,
  #                                              `selected-text-format` = "count > 1"),
  #                               multiple = T,
  #                               width = '90%',
  #                               selected = if (preset_selections$preset == "Environmental justice") {
  #                                 filter(metadata, type == "people", ej == 1) %>% .$name
  #                                 } else if (preset_selections$preset == "Public health") {
  #                                   filter(metadata, type == "people", ph == 1) %>% .$name
  #                                 } else if (preset_selections$preset == "Climate change") {
  #                                   filter(metadata, type == "people", cc == 1) %>% .$name
  #                                 } else if (preset_selections$preset == "Custom") {
  #                                   filter(metadata, type == "people") %>% .$name}))
  #   return(if (preset_selections$preset == "Custom") {a} else {a %>% shinyjs::disabled()})
  # })
  # 
  # 
  # output$placeInput <- renderUI({
  #   ns <- session$ns
  #   tagList(  
  #     a <- shinyWidgets::pickerInput(ns("placeInput"), 
  #                                    label = shiny::HTML(paste0("<h4><span style='font-size:14pt'>Environment & Climate</span></h4>")),
  #                                    choices = filter(metadata, type == "environment") %>% .$name,
  #                                    options = list(`actions-box` = TRUE,
  #                                                   size = 20,
  #                                                   `selected-text-format` = "count > 1"),
  #                                    multiple = T,
  #                                    width = '90%',
  #                                    selected = if (preset_selections$preset == "Environmental justice") {
  #                                      filter(metadata, type == "environment", ej == 1) %>% .$name
  #                                    } else if (preset_selections$preset == "Public health") {
  #                                      filter(metadata, type == "environment", ph == 1) %>% .$name
  #                                    } else if (preset_selections$preset == "Climate change") {
  #                                      filter(metadata, type == "environment", cc == 1) %>% .$name
  #                                    } else if (preset_selections$preset == "Custom") {
  #                                      filter(metadata, type == "environment") %>% .$name}))
  #   return(if (preset_selections$preset == "Custom") {a} else {a %>% shinyjs::disabled()})
  # })
  # 
  # output$treeInput <- renderUI({
  #   ns <- session$ns
  #   tagList(  
  #     
  #     a <- shinyWidgets::pickerInput(ns("treeInput"), 
  #                                    label = shiny::HTML(paste0("<h4><span style='font-size:14pt'>Existing tree canopy</span></h4>")),
  #                                    choices = filter(metadata, type == "tree") %>% .$name,
  #                                    options = list(`actions-box` = TRUE,
  #                                                   size = 20,
  #                                                   `selected-text-format` = "count > 1"),
  #                                    multiple = T,
  #                                    width = '90%',
  #                                    selected = if (preset_selections$preset == "Environmental justice") {
  #                                      filter(metadata, type == "tree", ej == 1) %>% .$name
  #                                    } else if (preset_selections$preset == "Public health") {
  #                                      filter(metadata, type == "tree", ph == 1) %>% .$name
  #                                    } else if (preset_selections$preset == "Climate change") {
  #                                      filter(metadata, type == "tree", cc == 1) %>% .$name
  #                                    } else if (preset_selections$preset == "Custom") {
  #                                      filter(metadata, type == "tree") %>% .$name}))
  #   return(if (preset_selections$preset == "Custom") {a} else {a %>% shinyjs::disabled()})
  # })
  # 
  # input_values <- reactiveValues() # start with an empty reactiveValues object.
  # observe({ input_values$allInputs <-  as_tibble(input$peopleInput) %>%
  #       rbind(as_tibble(input$placeInput)) %>%
  #       rbind(as_tibble(input$treeInput))
  # })
  # return(input_values)
  
}
  
  
    
## To be copied in the UI
# mod_map_selections_ui("map_selections_ui_1")
    
## To be copied in the server
# callModule(mod_map_selections_server, "map_selections_ui_1")

