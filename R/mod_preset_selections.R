#' preset_selections UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_preset_selections_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    fluidRow(radioButtons(
      ns("presetInput"),
      label = HTML("<h3>Step 1: Select priority variables</h3>
                                         <p>Start with a preset of variables and customize as needed. Resulting values for each tract ranges from 0-10, and represents an average of standardized and scaled raw values.</p>"),
      choices = c("Climate change", "Environmental justice", "Public health", "Custom"),
      selected = c("Environmental justice"),
      inline = TRUE))
    
  )
}
    
#' preset_selections Server Functions
#'
#' @noRd 
mod_preset_selections_server <- function(id){
  moduleServer( id, function(input, output, session,
                             current_tab = input$nav){
    ns <- session$ns

    input_values <- reactiveValues()
    observe({
      input_values$preset <- input$presetInput
    })
    
    return(input_values)

  })
}
    
## To be copied in the UI
# mod_preset_selections_ui("preset_selections_ui_1")
    
## To be copied in the server
# mod_preset_selections_server("preset_selections_ui_1")
