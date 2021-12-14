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

    # absolutePanel(
    #   id = "controls",
    #   class = "panel panel-default", 
    #   fixed = TRUE,
    #   draggable = TRUE, top = "13%", left = "50%", right = "auto", bottom = "auto",
    #   style = "padding: 7px",
    #   width = "auto", height = "auto",
    #   HTML('<button data-toggle="collapse" data-target="#report">Create report</button>'),
    #   tags$div(id = 'report',  class="collapse",#in
               
      radioButtons(
      ns("geo"),
      label = h3("Report region"),
      choices = c("Selected tract", "City (MetCouncil region only)"), #"County (within MN or WI)"),
      selected = c("Selected tract"),#c("City (MetCouncil region only)"),
      inline = F 
      ),
      
      conditionalPanel(
        ns = ns,
        condition = "input.geo == 'City (MetCouncil region only)'",
        
        shinyWidgets::pickerInput(ns("cityInput"), 
                                  label = shiny::HTML(paste0("<h4><span style='font-size:14pt'>City or township</span></h4>")),
                                  choices = ctus,
                                  options = list(size = 20),
                                  multiple = F,
                                  selected = "Afton" #filter(metadata, type == "people")[1, 2]
        )),
      # conditionalPanel(
      #   ns = ns,
      #   condition = "input.geo == 'County (within MN or WI)'",
      #   
      #   shinyWidgets::pickerInput(ns("stateInput"), 
      #                             label = shiny::HTML(paste0("<h4><span style='font-size:14pt'>Placeholder</span></h4>")),
      #                             choices = c("Minnesota", "Wisconsin"),
      #                             options = list(size = 5),
      #                             multiple = F,
      #                             selected = "Minnesota" #filter(metadata, type == "people")[1, 2]
      #   )),
      
    
    downloadButton(ns("tract_report"), "Generate a report")
      )
  # )
  # )
}
    
#' plot_tract Server Function
#'
#' @noRd 
#' @import ggplot2
mod_plot_tract_server <- function(input, output, session,
                                  tract_selections = tract_selections,
                                  map_util = map_util,
                                  map_selections = map_selections){
  ns <- session$ns
  
  # output$moreControls <- renderUI({
  #   HTML(paste0("a ", "testing"))
  # })

  
  
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
    # filename = paste0("GrowingShade_", Sys.Date(), ".pdf"), # ".html"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(selected_tract = tract_selections$selected_tract,
                     selected_geo = input$geo,
                     selected_city = input$cityInput,
                     vars_used = map_selections$preset,
                     priority_score = map_util$map_data2,#round(map_util$map_data2$MEAN, 3),
                     rank_total = nrow(map_util$map_data2),
                     vars_selected = map_selections$allInputs,
                     canopy = map_util$canopycov)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()),
                        # output_format = "pdf_document", #"html_document",
                        output_format = "html_document",
                        output_options = list(html_preview = FALSE,
                                              toc = TRUE, 
                                              # theme = "cosmo",
                                              toc_depth = 3,
                                              fig_caption = TRUE
                                              # theme = NULL,
                                              # css = system.file("app/www/style.css", package = 'planting.shade')
                                              
                                              ),
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
 
