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
      column(width = 4, uiOutput(ns("peopleoptions"))),
      # column(width = 4, shinyWidgets::pickerInput(ns("peopleInput"),
      #                                                    width = '90%',
      #                         label = shiny::HTML(paste0("<h4>Equity & People</h4>", "<p style='font-size:16px'>Variables about people.</p>")), # style='font-size:20px' 'color=#0054A4'>
      #                         choices = if (preset_selections$preset == "Environmental justice") {
      #                           filter(eva_vars, type == "people", ej ==1)$name
      #                           } else {filter(eva_vars, type == "people")$name}, 
      #                         options = list(`actions-box` = TRUE, 
      #                                        size = 10,
      #                                        `selected-text-format` = "count > 1"), 
      #                         multiple = T,
      #                           )),
    # hr(),
    column(width = 4, shinyWidgets::pickerInput(ns("placeInput"),
                                                width = '90%',
                              label = shiny::HTML("<h4>Infrastructure & Environment</h4>", "<p style='font-size:16px'>Variables about the natural or built environment.</p>"), 
                              choices=filter(eva_vars, type == "environment")$name, 
                              options = list(`actions-box` = TRUE, 
                                             size = 10,
                                             `selected-text-format` = "count > 1"), 
                              multiple = T#,
                              )),
    
    # hr(),
    # 
    column(width=4,shinyWidgets::pickerInput(ns("treeInput"),
                                             width = '90%',
                              label = shiny::HTML("<h4>Existing tree canopy</h4>", "<p style='font-size:16px'>Variables about the existing tree canopy.</p>"), 
                              choices=filter(eva_vars, type == "tree")$name, 
                              options = list(`actions-box` = TRUE, 
                                             size = 10,
                                             `selected-text-format` = "count > 1"), 
                              multiple = T
                              ))
    ),
    # radioButtons(ns("weight"),
    #              label = shiny::HTML("<h4>Variable weights</h4>", "<p style='font-size:16px'>Choose how tract scores are calculated.</p>"),
    #              choices = c("Weight all variables equally", "Weight all categories equally"),
    #              selected = "Weight all variables equally",
    #              inline = T), br(),
    
    # hr(),
    # shinyWidgets::pickerInput(ns("businessInput"),
    #                           label = shiny::HTML("<h3>Resilience & Business</h3>", "<p style='font-size:16px'>Some description about this group of variables.</p>"), 
    #                           choices=filter(eva_vars, type == "business")$name, 
    #                           options = list(`actions-box` = TRUE, 
    #                                          size = 10,
    #                                          `selected-text-format` = "count > 1"),
    #                           multiple = T,
    #                           selected = filter(eva_vars, type == "business")$name),
    # 
    # hr(),
    actionButton(ns("goButton"), "Update map", class = "btn-success", style='padding:7px; font-size:16px'),
    
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
  
  output$peopleoptions <- renderUI({
    ns <- session$ns
    tagList(  
      test <- shinyWidgets::pickerInput(ns("peopleoptions"), 
                                label = shiny::HTML(paste0("<h3>People & Equity</h3>")),
                                choices = filter(metadata, type == "people") %>% .$name,
                                options = list(`actions-box` = TRUE,
                                               size = 20,
                                               `selected-text-format` = "count > 1"),
                                multiple = T,
                                width = '90%',
                                selected = if (preset_selections$preset == "Environmental justice") {
                                  filter(metadata, type == "people", ej == 1) %>% .$name
                                  } else {filter(metadata, type == "people") %>% .$name}))
      return(if (preset_selections$preset == "Custom") {test} else {test %>% shinyjs::disabled()})
      
    
  })

  
  # observe({
  #   if (preset_selections$preset == "Public health") {
  #     shinyjs::disable("peopleoptions")
  #   } else {
  #     shinyjs::enable("peopleoptions")
  #   }
  # })
  
  input_values <- reactiveValues() # start with an empty reactiveValues object.
  
  observeEvent(input$goButton,{
      input$goButton
    
    input_values$presetInput <- input$presetInput
    input_values$peopleInput <- input$peopleInput
    input_values$placeInput <- input$placeInput
    input_values$treeInput <- input$treeInput
    
    input_values$allInputs <- as_tibble(input$peopleInput) %>%
      rbind(as_tibble(input$placeInput)) %>%
      rbind(as_tibble(input$treeInput)) %>%
      rbind(as_tibble(input$presetInput))
  }, ignoreNULL = FALSE)
  
  return(input_values)
  
}
    
## To be copied in the UI
# mod_map_selections_ui("map_selections_ui_1")
    
## To be copied in the server
# callModule(mod_map_selections_server, "map_selections_ui_1")

