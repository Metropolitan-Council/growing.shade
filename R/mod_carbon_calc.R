#' carbon_calc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_carbon_calc_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' carbon_calc Server Function
#'
#' @noRd 
mod_carbon_calc_server <- function(input, output, session){
  ns <- session$ns
  
  tibble(species = rep(c("oak", "maple"), each = 100),
         age = rep(c(1:100), 2),
         diameter = rep(c(1:20, seq(20.5, 30, by = .5), seq(30.5, 40, by = .16)), 2)) %>%
    mutate(diameter = if_else(species == "oak", diameter/.7, diameter)) %>%
    mutate(stock = diameter / .5, 
           sequestration = diameter / 10) %>%
    pivot_longer(names_to = "measurement", values_to = "values", -c("species", "age")) %>%
    
    ggplot(aes(x = age, y = values, col = measurement)) +
    geom_line() +
    facet_wrap(~species)
 
}
    
## To be copied in the UI
# mod_carbon_calc_ui("carbon_calc_ui_1")
    
## To be copied in the server
# callModule(mod_carbon_calc_server, "carbon_calc_ui_1")
 
