#' plot_tract UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_tract_ui <- function(id){
  ns <- NS(id)
  tagList(
    # fluidRow(h3("Step 3: Make a report"),
    #          p("This tool is primarily intended to identify locations where new tree plantings could have disproportionately positive impacts. However, there is more detail about trees across the region. Please generate a report if you are in need of more detail about a specific area.")),
    # fluidRow(
      radioButtons(
      ns("geo"),
      label = h3("Report region"),
      choices = c("Selected tract from above", "A specific city", "Greater MN and WI"),
      selected = c("Selected tract from above"),
      inline = F 
      ),
    
    downloadButton(ns("tract_report"), "Generate a report")
    
    # plotOutput(ns("bargraph"), height = 900)
    
  )
}
    
#' plot_tract Server Function
#'
#' @noRd 
#' @import ggplot2
mod_plot_tract_server <- function(input, output, session,
                                  tract_selections = tract_selections,
                                  map_util = map_util){
  ns <- session$ns
  

  # make_plot_vals <-  reactive({
  #   selected_tract <- map_util$plot_data2 %>%
  #     ungroup() %>%
  #     filter(tract_string == tract_selections$selected_tract) %>%
  #     rename(SCALED_WTS = weights_scaled,
  #            RAW = raw_value) %>%
  #     mutate(dsource = "Selected tract") %>%
  #     select(name, SCALED_WTS, RAW, dsource) 
  #   
  #   tract_avgs <- map_util$plot_data2 %>%
  #     # eva_data_main %>%
  #     ungroup() %>%
  #     group_by(name) %>% 
  #     summarise(SCALED_WTS = mean(weights_scaled, na.rm = T),
  #               RAW = mean(raw_value, na.rm = T)) %>%
  #     mutate(dsource = "All tracts \n(average)")
  #   
  #   toplot <- bind_rows(selected_tract, tract_avgs)# %>%
  #     # gather(key = "key", value = "value", -name, -dsource)
  #   
  #   return(toplot)
  # })
  # 
  # 
  output$tract_report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    # filename = "report.html",
    filename = paste0("GrowingShade_", Sys.Date(), ".html"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(selected_tract = tract_selections$selected_tract)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  # 
  # output$bargraph <- renderPlot(#height = function() PlotHeight(), 
  #   {
  #   if(identical(tract_selections$selected_tract, character(0))) {
  #     print("nodata")
  #     ggplot() +
  #       theme_void()# +
  #       # annotate("text", x =0, y=0, label = "Click on a tract to view the values ")
  #     } else {
  #     print("making graph")
  #   ggplot() + 
  #     # geom_point(aes(x = ZSCORE, y = name, col = dsource), data = make_plot_vals()) +
  #     geom_point(aes(x = SCALED_WTS, y = name, col = dsource), data = make_plot_vals(),
  #              position = position_dodge(width = .2),
  #              size = 4) +
  #     scale_color_manual(values = c("#636363", ##0054A4", 
  #                                   "#78A22F"), name = "Legend:") +
  #     labs(y = "", x = "Score\n(high score = large opportunity)") +
  #     ggtitle(paste0("Summary for tract ", tract_selections$selected_tract)) +
  #   council_theme() +
  #     theme(axis.text.y = element_text(size = 15),
  #           axis.text.x = element_text(size = 15),
  #           legend.text = element_text(size = 15))+
  #     # scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 60)) +
  #     xlim(0, 10) +
  #     # xlim(min(map_util$map_data2$MEAN, na.rm = T), max(map_util$map_data2$MEAN, na.rm = T)) +
  #   theme(legend.position = "top")
  #   } 
  # })
  
  
  
  
}


    
## To be copied in the UI
# mod_plot_tract_ui("plot_tract_ui_1")
    
## To be copied in the server
# callModule(mod_plot_tract_server, "plot_tract_ui_1")
 
