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
    
    fluidRow(
      column(width = 4, uiOutput(ns("peopleInput"))),
      column(width = 4, uiOutput(ns("placeInput"))),
      column(width = 4, uiOutput(ns("treeInput"))))#,
    # radioButtons(ns("weight"),
    #              label = shiny::HTML("<h4>Variable weights</h4>", "<p style='font-size:16px'>Choose how tract scores are calculated.</p>"),
    #              choices = c("Weight all variables equally", "Weight all categories equally"),
    #              selected = "Weight all variables equally",
    #              inline = T), br(),
    
    # 
    # hr(),
    
    # actionButton(ns("goButton"), "Update map", class = "btn-success", style='padding:7px; font-size:16px'),
    
    # shiny::h4("Selected variables"),
    # textOutput(ns("selectedvars0")), #if want to print variables on shiny this works
    
    # textOutput(ns("selectedvars25"))
    

  )
}
    
#' map_selections Server Function
#'
#' @noRd 
mod_map_selections_server <- function(input, output, session,
                                      preset_selections,
                                      current_tab){
  ns <- session$ns
  
  output$peopleInput <- renderUI({
    ns <- session$ns
    tagList(  
      a <- shinyWidgets::pickerInput(ns("peopleInput"), 
                                label = shiny::HTML(paste0("<h4>People & Equity</h4>")),
                                choices = filter(metadata, type == "people") %>% .$name,
                                options = list(`actions-box` = TRUE,
                                               size = 20,
                                               `selected-text-format` = "count > 1"),
                                multiple = T,
                                width = '90%',
                                selected = if (preset_selections$preset == "Environmental justice") {
                                  filter(metadata, type == "people", ej == 1) %>% .$name
                                  } else if (preset_selections$preset == "Public health") {
                                    filter(metadata, type == "people", ph == 1) %>% .$name
                                  } else if (preset_selections$preset == "Climate change") {
                                    filter(metadata, type == "people", cc == 1) %>% .$name
                                  } else if (preset_selections$preset == "Custom") {
                                    filter(metadata, type == "people") %>% .$name}))
    return(if (preset_selections$preset == "Custom") {a} else {a %>% shinyjs::disabled()})
  })
  
  
  output$placeInput <- renderUI({
    ns <- session$ns
    tagList(  
      a <- shinyWidgets::pickerInput(ns("placeInput"), 
                                     label = shiny::HTML(paste0("<h4>Environment & Climate</h4>")),
                                     choices = filter(metadata, type == "environment") %>% .$name,
                                     options = list(`actions-box` = TRUE,
                                                    size = 20,
                                                    `selected-text-format` = "count > 1"),
                                     multiple = T,
                                     width = '90%',
                                     selected = if (preset_selections$preset == "Environmental justice") {
                                       filter(metadata, type == "environment", ej == 1) %>% .$name
                                     } else if (preset_selections$preset == "Public health") {
                                       filter(metadata, type == "environment", ph == 1) %>% .$name
                                     } else if (preset_selections$preset == "Climate change") {
                                       filter(metadata, type == "environment", cc == 1) %>% .$name
                                     } else if (preset_selections$preset == "Custom") {
                                       filter(metadata, type == "environment") %>% .$name}))
    return(if (preset_selections$preset == "Custom") {a} else {a %>% shinyjs::disabled()})
  })
  
  output$treeInput <- renderUI({
    ns <- session$ns
    tagList(  
      
      a <- shinyWidgets::pickerInput(ns("treeInput"), 
                                     label = shiny::HTML(paste0("<h4>Existing tree canopy</h4>")),
                                     choices = filter(metadata, type == "tree") %>% .$name,
                                     options = list(`actions-box` = TRUE,
                                                    size = 20,
                                                    `selected-text-format` = "count > 1"),
                                     multiple = T,
                                     width = '90%',
                                     selected = if (preset_selections$preset == "Environmental justice") {
                                       filter(metadata, type == "tree", ej == 1) %>% .$name
                                     } else if (preset_selections$preset == "Public health") {
                                       filter(metadata, type == "tree", ph == 1) %>% .$name
                                     } else if (preset_selections$preset == "Climate change") {
                                       filter(metadata, type == "tree", cc == 1) %>% .$name
                                     } else if (preset_selections$preset == "Custom") {
                                       filter(metadata, type == "tree") %>% .$name}))
    return(if (preset_selections$preset == "Custom") {a} else {a %>% shinyjs::disabled()})
  })

  
  # observe({
  #   if (preset_selections$preset == "Public health") {
  #     shinyjs::disable("peopleoptions")
  #   } else {
  #     shinyjs::enable("peopleoptions")
  #   }
  # })
  
  input_values <- reactiveValues() # start with an empty reactiveValues object.
  observe({ input_values$allInputs <-  as_tibble(input$peopleInput) %>%
        rbind(as_tibble(input$placeInput)) %>%
        rbind(as_tibble(input$treeInput))
    # as_tibble(
    # input$peopleInput) %>%
    #     rbind(as_tibble(input$placeInput)) %>%
    #     rbind(as_tibble(input$treeInput)) 
  })
  # observeEvent(input$goButton,{
  #     input$goButton
  #   
  #   input_values$presetInput <- input$presetInput
  #   input_values$peopleInput <- input$peopleInput
  #   input_values$placeInput <- input$placeInput
  #   input_values$treeInput <- input$treeInput
  #   
  #   input_values$allInputs <- as_tibble(input$peopleInput) %>%
  #     rbind(as_tibble(input$placeInput)) %>%
  #     rbind(as_tibble(input$treeInput)) %>%
  #     rbind(as_tibble(input$presetInput))
  # }, ignoreNULL = FALSE)
  # 
  return(input_values)
  
}
    
## To be copied in the UI
# mod_map_selections_ui("map_selections_ui_1")
    
## To be copied in the server
# callModule(mod_map_selections_server, "map_selections_ui_1")

