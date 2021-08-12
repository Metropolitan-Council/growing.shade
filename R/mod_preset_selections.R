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

    p("While the tool is fully customizable, regional priority layers have been identified for key presets. Select 'custom' to use the 3 dropdown menus."),
    fluidRow(column(width = 12, 
    radioButtons(
      ns("presetInput"),
      label = h4("Preset"),
      choices = c("Climate change", "Environmental justice", "Public health", "Custom"),
      selected = c("Environmental justice"),
      inline = F)))
    
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
