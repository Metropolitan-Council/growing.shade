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
    fluidRow(column(width = 6, radioButtons(
      ns("presetInput"),
      label = h3("Step 1: Select priority"),
      choices = c("Climate change", "Environmental justice", "Public health", "Custom"),
      selected = c("Environmental justice"),
      inline = TRUE) %>% 
        shinyhelper::helper(
          type = "markdown",
          content = "PresetHelp"
        )))
    
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
