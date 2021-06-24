#' story_generator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_story_generator_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Story generator - option for showing personal stories"),
    br(),
    fluidRow(column(width = 4, 
                    p("Trees intersect with many other topics of regional importance. Here are some stories and ideas to better contextualize this work and to inspire imagination at what the future could look like."),
                    br(),
                    p("Click on a topic area to see stories about trees within that context. We'd love to add your story or topic, please contact us if you're willing to do an interview or if you have a news story to share!")),
             # br(),
    # fluidRow(
    column(width = 4, align = "top",
                    radioButtons(ns("radio"), label = h3("Show stories about trees and"),
                                 choices = story_topic_vars$topic,
                                 selected = "Agriculture",
                                 width = "100%")),
             column(width = 4,
                    uiOutput(ns("make_storysample"))))
  )
}
    
#' story_generator Server Function
#'
#' @noRd 
mod_story_generator_server <- function(input, output, session){
  ns <- session$ns
 
  output$make_storysample <- renderUI({#reactive({
    req(input$radio)

    story <- readxl::read_xlsx("./data/story generator.xlsx") %>%
      filter(topic == input$radio) #%>%
      # sample_n(3) 
    return(HTML(paste0("<p>", story$story, "</p><hr>"#,
                       # "<p>", story$story, "</p><hr>",
                       # "<p>", story$story, "</p>"
                       )))
    # }
  })
  
  
}
    
## To be copied in the UI
# mod_story_generator_ui("story_generator_ui_1")
    
## To be copied in the server
# callModule(mod_story_generator_server, "story_generator_ui_1")
 
