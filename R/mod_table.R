#' table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    dataTableOutput(ns("table"))
    
  )
}
    
#' table Server Function
#'
#' @noRd 
mod_table_server <- function(input, output, session,
                             tract_selections = tract_selections,
                             map_util = map_util){
  ns <- session$ns
 
  make_table_vals <-  reactive({

    selected_tract <- map_util$plot_data2 %>%
      filter(tract_string == tract_selections$selected_tract) %>%
      # eva_data_main %>%
      ungroup() %>%
      select(name, raw_value, overall_rank, COUNT)  %>%
      mutate(raw_value= round(raw_value, 3)) %>%
      mutate_if(is.numeric, format, big.mark = ",")  %>%
      rename(`Selected tract` = raw_value,
             `Rank of variable` = overall_rank,
             `Total tracts with data` = COUNT)
    
    avg_tracts <- #
      # eva_data_main %>% 
      map_util$plot_data2 %>%
      ungroup() %>%
      group_by(name) %>%
      summarise(`Average tract`= round(mean(raw_value, na.rm = T), 3)) %>%
      mutate_if(is.numeric, format, big.mark = ",") 
    
    fulltable <- full_join(avg_tracts, selected_tract, by = "name") %>%
      rename(`Variable name` = name)
    
    return(fulltable)
  })
  
  output$table <- renderDataTable({
    if(identical(tract_selections$selected_tract, character(0))) {
      print("nodata")
      tibble()
    } else {
      (make_table_vals()
       # options = list(pageLength =5)
       )
    }
  })
  
  
  
  
  
}
    
## To be copied in the UI
# mod_table_ui("table_ui_1")
    
## To be copied in the server
# callModule(mod_table_server, "table_ui_1")
 
# iamrenny@yahoo.com.

# kellerab@umn.edu

# Ellen, there is a great story from the Creative Enterprise Zone’s 100 Tree Project. It involves planting 100 boulevard trees in South St. Anthony Park. The lead person is Ben Shardlow benshardlow@gmail.com https://creativeenterprisezone.org/trees

# I really like some of the examples on the vibrant cities lab website: https://www.vibrantcitieslab.com/

# Another kind of unique example: https://www.climatehubs.usda.gov/hubs/northeast/project/worcesters-urban-forest

# http://www.urbangreenforpeople.com/heritagetrees.html

# Oh - I also wanted and forgot to mention that there’s a huge timescale challenge here when we are talking about growing shade through planting trees - people have to really be thinking about and imagining their neighborhoods 20+ years in the future, which will also be a challenge/opportunity, especially for communities with concerns of gentrification/displacement or many renters



#need agency in making decisions

# •	Land Acknowledgement - We want to acknowledge that we gather on the traditional land of the Dakota people and honor with gratitude the land itself and the people who have stewarded it throughout the generations, including the Ojibwe and other indigenous nations. We recognize and respect the enduring relationship that exists between many Indigenous peoples and their traditional homelands. Despite centuries of colonial theft and violence, this is still and always will be Indigenous land. Indigenous people are still here, demonstrating innumerable talents and gifts in the midst of continued oppression and colonialism. Indigenous people must be celebrated and uplifted.
