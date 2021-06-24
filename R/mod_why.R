#' why UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_why_ui <- function(id){
  ns <- NS(id)
  tagList(
    # <section class='bannergeneric' style='background:url(./www/3789.png); background-size:cover; height=100%;'><br><br>
      
    fluidRow(
      HTML("
                                     
                                     <section class='banner' style='background:url(./www/3789.png)'><br><br>
                                     <h1 class='parallax' style='color:#78A22F; background-color: #ffffff; opacity: 0.8; padding: 1px'>Why plant shade?</h1>
                                     <p class='parallax_description'>Many reasons</p>
                                     </section>
                                     ")
    ),
 
  )
}
    
#' why Server Function
#'
#' @noRd 
mod_why_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_why_ui("why_ui_1")
    
## To be copied in the server
# callModule(mod_why_server, "why_ui_1")
 
