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
    
    # fluidRow(radioButtons(
    #                         ns("presetInput"),
    #                         label = HTML("<h3>Step 1: Select priority variables</h3>
    #                                      <p>Select a preset or 'custom' variables and click 'update map.' Resulting values for each tract ranges from 0-10, and represents an average of standardized and scaled raw values.</p>"),
    #                         choices = c("Climate change" = "cc", "Environmental justice", "Public health", "Custom"),
    #                         selected = c("Environmental justice"),
    #                         inline = TRUE)),
    
    fluidRow(column(width = 4, shinyWidgets::pickerInput(ns("peopleInput"),
                                                         width = '90%',
                              label = shiny::HTML(paste0("<h4>Equity & People</h4>", "<p style='font-size:16px'>Variables about people.</p>")), # style='font-size:20px' 'color=#0054A4'>
                              choices = filter(eva_vars, type == "people")$name, 
                              options = list(`actions-box` = TRUE, 
                                             size = 10,
                                             `selected-text-format` = "count > 1"), 
                              multiple = T,
                              # selected = peopleoptions$opts
                              
                                
                              #   if (ns("presetInput") == "Climate change") {
                              #   dplyr::filter(eva_vars, type == "people", cc == 1)$name
                              # } else if (ns("presetInput") == "Environmental justice"){
                              #   dplyr::filter(eva_vars, type == "people", ej == 1)$name
                              # } else if (ns("presetInput") == "Public health") {
                              #   dplyr::filter(eva_vars, type == "people", ph == 1)$name
                              # } else if (ns("presetInput") == "Custom") {
                              #   dplyr::filter(eva_vars, type == "people")$name
                              # }
                                )),
    # hr(),
    column(width = 4, shinyWidgets::pickerInput(ns("placeInput"),
                                                width = '90%',
                              label = shiny::HTML("<h4>Infrastructure & Environment</h4>", "<p style='font-size:16px'>Variables about the natural or built environment.</p>"), 
                              choices=filter(eva_vars, type == "environment")$name, 
                              options = list(`actions-box` = TRUE, 
                                             size = 10,
                                             `selected-text-format` = "count > 1"), 
                              multiple = T#,
                              # selected = if (ns("presetInput") == "Climate change") {
                              #   filter(eva_vars, type == "environment", cc == 1)$name
                              # } else if (ns("presetInput") == "Environmental justice"){
                              #   filter(eva_vars, type == "environment", ej == 1)$name
                              # } else if (ns("presetInput") == "Public health") {
                              #   filter(eva_vars, type == "environment", ph == 1)$name
                              # } else if (ns("presetInput") == "Custom") {
                              #   filter(eva_vars, type == "environment")$name
                              # }
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
                              multiple = T#,
                              # selected = if (ns("presetInput") == "Climate change") {
                              #   filter(eva_vars, type == "tree", cc == 1)$name
                              # } else if (ns("presetInput") == "Environmental justice"){
                              #   filter(eva_vars, type == "tree", ej == 1)$name
                              # } else if (ns("presetInput") == "Public health") {
                              #   filter(eva_vars, type == "tree", ph == 1)$name
                              # } else if (ns("presetInput") == "Custom") {
                              #   filter(eva_vars, type == "tree")$name
                              # }
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
  
  #uncomment if want to print variables included
  # output$selectedvars0 <- renderText({
  #   input$goButton
  #   a <- isolate(input$peopleInput)
  #   b <- isolate(input$placeInput)
  #   c <- isolate(input$businessInput)
  #   toprint <- paste(a, b, c, sep = "; ")
  #   toprint
  #   })
  
  # output$selectedvars25 <- renderText(input$peopleInput %>% rbind(input$placeInput))
  
  # peopleoptions <- reactiveValues()
  # observeEvent(preset_selections$presetInput,{
  #   peopleoptions$opts <- if (ns("presetInput") == "Climate change") {
  #       filter(eva_vars, type == "people", cc == 1)$name
  #     } else if (ns("presetInput") == "Environmental justice"){
  #       filter(eva_vars, type == "people", ej == 1)$name
  #     } else if (ns("presetInput") == "Public health") {
  #       filter(eva_vars, type == "people", ph == 1)$name
  #     } else if (ns("presetInput") == "Custom") {
  #       filter(eva_vars, type == "people")$name
  #     }
  # })
  # return(peopleoptions)
  
  
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
  
  
  # observeEvent(input$peopleInput, { # only update when the user changes the eva input
  #   input_values$peopleInput <- input$peopleInput # create/update the eva input value in our reactiveValues object
  # })
  # 
  # observeEvent(input$placeInput, { # only update when the user changes the eva input
  #   input_values$placeInput <- input$placeInput # create/update the eva input value in our reactiveValues object
  # })
  # 
  # observeEvent(input$businessInput, { # only update when the user changes the eva input
  #   input_values$businessInput <- input$businessInput # create/update the eva input value in our reactiveValues object
  # })
  
  return(input_values)
  
}
    
## To be copied in the UI
# mod_map_selections_ui("map_selections_ui_1")
    
## To be copied in the server
# callModule(mod_map_selections_server, "map_selections_ui_1")

