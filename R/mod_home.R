#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    tabPanel("HOME", value = "home",
             shinyjs::useShinyjs(),
             tags$head(tags$script(HTML('
                                                       var fakeClick = function(tabName) {
                                                       var dropdownList = document.getElementsByTagName("a");
                                                       for (var i = 0; i < dropdownList.length; i++) {
                                                       var link = dropdownList[i];
                                                       if(link.getAttribute("data-value") == tabName) {
                                                       link.click();
                                                       };
                                                       }
                                                       };
                                                       ')))),

    
    fluidRow(
      HTML("
                                     
                                     <section class='banner'><br><br>
                                     <h1 class='parallax'>Where to plant shade?</h1>
                                     <p class='parallax_description'>Starting a conversation around tree canopy prioritization in the seven county Twin Cities region</p>
                                     </section>
                                     ")
    ),
  
    
    
  )
}
    
#' home Server Function
#'
#' @noRd 
mod_home_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_home_ui("home_ui_1")
    
## To be copied in the server
# callModule(mod_home_server, "home_ui_1")
 
