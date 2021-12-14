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
    
    # absolutePanel(
    #   id = "controls",
    #   class = "panel panel-default", 
    #   fixed = TRUE,
    #   draggable = TRUE, top = "13%", left = "5%", right = "auto", bottom = "auto",
    #   style = "padding: 7px; z-index: 900",
    #   width = "auto", height = "auto",
    #   HTML('<button data-toggle="collapse" data-target="#demo">Customizations</button>'),
    #   tags$div(id = 'demo',  class="collapse in",
               
    
    # radioButtons(ns("onoff"),
    #              HTML("<h3>Prioritization layer</h3><p><section style='font-weight: normal;'>The region's tree canopy intersects with regional issues and priorities such as climate resilience, equity, and health. Turn on/off this layer to show the intersection of the tree canopy with other factors. </section></p><br>"),
    #              choices = c("On", "Off"),
    #              selected = "On",
    #              inline = T),  
    
    
    
    # conditionalPanel(
      # ns = ns,
      # condition = "input.onoff == 'On'",
      radioButtons(ns("preset"), 
                   HTML("<h3>Prioritization layer</h3><p><section style='font-weight: normal;'>Trees intersect with regional issues and priorities. Select a priority layer to understand the overlap.</section></p><br>"),
                   # h4("Preset"),
                  choices = c(
                    "Climate change",
                    "Conservation",
                    "Environmental justice",
                    "Public health",
                    "Custom"
                  ), inline= T,
                  selected = "Environmental justice"
      ) %>%
        shinyhelper::helper(type = "markdown", content = "PresetHelp", size = "l"
                            # )
      ),
      
      conditionalPanel(
        ns = ns,
        condition = "input.preset == 'Custom'", # && input.onoff == 'On'",
        
        shinyWidgets::pickerInput(ns("peopleInput"), 
                                  label = shiny::HTML(paste0("<h4><span style='font-size:14pt'>Demographics</span></h4>")),
                                  choices = filter(metadata, type == "people") %>% .$name,
                                  # choicesOpt = list(
                                  #   subtext = paste0(filter(metadata, type == "people") %>% .$niceinterp, 
                                  #                    " values have higher priority scores")),
                                  options = list(`actions-box` = TRUE,
                                                 size = 10,
                                                 `selected-text-format` = "count > 1"),
                                  multiple = T,
                                  selected = NULL #filter(metadata, type == "people")[1, 2]
        )),
        conditionalPanel(
          ns = ns,
          condition = "input.preset == 'Custom'", # && input.onoff == 'On'",
          
          shinyWidgets::pickerInput(ns("placeInput"), 
                                    label = shiny::HTML(paste0("<h4><span style='font-size:14pt'>Environment & Climate</span></h4>")),
                                    choices = filter(metadata, type == "environment") %>% .$name,
                                    choicesOpt = list(
                                      subtext = paste0(filter(metadata, type == "environment") %>% .$nicer_interp)
                                    ),
                                    # choicesOpt = list(
                                    #   subtext = paste0(filter(metadata, type == "environment") %>% .$niceinterp, 
                                    #                    " values have higher priority scores")),
                                    options = list(`actions-box` = TRUE,
                                                   size = 20,
                                                   `selected-text-format` = "count > 1"),
                                    multiple = T,
                                    selected = filter(metadata, type == "environment")[7, 2]
          ))
      ,conditionalPanel(
        ns = ns,
        condition = "input.preset == 'Custom'", # && input.onoff == 'On'",
        
        shinyWidgets::pickerInput(ns("healthInput"),
                                  label = shiny::HTML(paste0("<h4><span style='font-size:14pt'>Health</span></h4>")),
                                  choices = filter(metadata, type == "health") %>% .$name,
                                  # choicesOpt = list(
                                  #   subtext = paste0(filter(metadata, type == "health") %>% .$niceinterp,
                                  #                     " values have higher priority scores")),
                                  options = list(`actions-box` = TRUE,
                                                 size = 20,
                                                 `selected-text-format` = "count > 1"),
                                  multiple = T,
                                  selected = NULL
        )
      )
      
      ,conditionalPanel(
        ns = ns,
        condition = "input.preset == 'Custom'", # && input.onoff == 'On'",
        
        shinyWidgets::pickerInput(ns("dollarInput"),
                                  label = shiny::HTML(paste0("<h4><span style='font-size:14pt'>Socioeconomics</span></h4>")),
                                  choices = filter(metadata, type == "dollar") %>% .$name,
                                  # choicesOpt = list(
                                  #   subtext = paste0(filter(metadata, type == "dollar") %>% .$niceinterp,
                                  #                    " values have higher priority scores")),
                                  options = list(`actions-box` = TRUE,
                                                 size = 20,
                                                 `selected-text-format` = "count > 1"),
                                  multiple = T,
                                  selected = NULL
        )
      )

      )

}
    
#' map_selections Server Function
#'
#' @noRd 
mod_map_selections_server <- function(input, output, session,
                                      # preset_selections,
                                      current_tab){
  ns <- session$ns
  
  input_values <- reactiveValues() # start with an empty reactiveValues object.
  
  observe({ input_values$allInputs <- as_tibble(input$peopleInput) %>%
        rbind(as_tibble(input$placeInput)) %>%
        rbind(as_tibble(input$healthInput)) %>%
    rbind(as_tibble(input$dollarInput))
  
  # input_values$priority_layer <- input$onoff
  
  input_values$preset <- input$preset
  })
  return(input_values)

  
}
  
  
    
## To be copied in the UI
# mod_map_selections_ui("map_selections_ui_1")
    
## To be copied in the server
# callModule(mod_map_selections_server, "map_selections_ui_1")

