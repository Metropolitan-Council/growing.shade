#' storymap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_storymap_ui <- function(id) {
  ns <- NS(id)

  
  tagList(
    # tags$head(tags$style(HTML(hiding_js))),
    # tags$style(type = "text/css", "#storymap {overflow:hidden}"),
    # div(br(class = "d-none d-lg-block")),
    bootstrapPage(# theme  = bslib::bs_theme(version = 5),
    # h2("Old Faithful Geyser Data", class="d-none d-lg-block"),
    # h2("Geyser Data", class="d-block d-lg-none"),
      br(class="d-none d-lg-block"),
      # htmlOutput(ns("storymap_wide"), class = "d-none d-lg-block"),
      # htmlOutput(ns("storymap_narrow"), class = "d-block d-lg-none")
      htmlOutput(ns("storymap"))
  )
  )
}

#' storymap Server Functions
#'
#' @noRd
mod_storymap_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$storymap <- renderUI({
      # my_test <- HTML("<div style='max-width:3000px'><body><iframe title='StoryMap about the Growing Shade Project' src='https://storymaps.arcgis.com/stories/1a7fe9365a8f4bfc81305bb288fbbcfa' style='width:100%; top:150px !important; bottom:0 !important;padding:0;height:100vh;border:none; frameborder = '0'</iframe></body></div>")
      my_test <- HTML("<div style='max-width:3000px;'><body><iframe title='StoryMap about the Growing Shade Project' src='https://storymaps.arcgis.com/stories/1a7fe9365a8f4bfc81305bb288fbbcfa' style='width:100%; height:calc(90vh); padding:0;border:0; object-position: center bottom;'</iframe></body></div>")

      # margin vs bottom
      # height 88vh
      # calc(100vh - 120px) #generally, this is working well!! but need to use 88vh to make work okay on mobile

      my_test
    })
    
    
    # output$storymap_narrow <- renderUI({
    #   # my_test <- HTML("<div style='max-width:3000px'><body><iframe title='StoryMap about the Growing Shade Project' src='https://storymaps.arcgis.com/stories/1a7fe9365a8f4bfc81305bb288fbbcfa' style='width:100%; top:150px !important; bottom:0 !important;padding:0;height:100vh;border:none; frameborder = '0'</iframe></body></div>")
    #   my_test <- HTML("<div style='max-width:3000px;'><body><iframe title='StoryMap about the Growing Shade Project' src='https://storymaps.arcgis.com/stories/1a7fe9365a8f4bfc81305bb288fbbcfa' style='width:100%; height:calc(100vh - 120px); padding:0;border:0; object-position: center bottom;'</iframe></body></div>")
    #   
    #   # margin vs bottom
    #   # height 88vh
    #   # calc(100vh - 80px)
    #   
    #   my_test
    # })
    
    

  })
}

## To be copied in the UI
# mod_storymap_ui("storymap_ui_1")

## To be copied in the server
# mod_storymap_server("storymap_ui_1")
