#' video UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_video_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    h2("History - option for including multimedia"),
    HTML('<div position="relative" overflow="hidden" width="500" height="500"><iframe position="absolute" width="100vw" height="100vh" src="https://www.youtube.com/embed/b4eLTYUcj7k?start=10&controls=0&showinfo=0&rel=0&autoplay=1&loop=1&mute=1" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></div>'), 
                                     
  )
}
    
#' video Server Function
#'
#' @noRd 
mod_video_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_video_ui("video_ui_1")
    
## To be copied in the server
# callModule(mod_video_server, "video_ui_1")
 
