#' intro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_stories_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Story map"),
    br(),
    fluidRow(column(width = 5, 
                    p("Growing shade looks differently across the region. Here are some stories and ideas to better contextualize this work and to inspire imagination at what the future could look like."),
                    br(),
                    p("Move around and zoom on the map below to explore regional stories. Click icons for more information about all the cool stuff happening here. We'd love to add your story to the map, please contact us if you're willing to do an interview or if you have a news story to share!")),
             column(width = 7, align = "center",
                    leafletOutput(ns("storymap"), height = 600, width = 600)))
    
    # p("Growing shade looks differently across the region. Here are some stories and ideas to better contextualize this work and to inspire imagination at what the future could look like."),
    # br(),
    # p("Move around and zoom on the map below to explore regional stories. Click icons for more information about all the cool stuff happening here. We'd love to add your story to the map, please contact us if you're willing to do an interview or if you have a news story to share!"),
    # br(),
    # fluidRow(column(width = 12),
    #          style = "background-color:#0054A4;",
    #          HTML("<h3 style='color:white;'>Region-wide</h3>")),
    # fluidRow(column(width = 4,
    #                 HTML("<center><img src='www/logging.png' height='200px' class = 'circimg' alt='old sawmill picture'/img></center>")),
    #          column(width = 4,
    #                 HTML('<div style="position: absolute; bottom: 0; left: 0.5em; width: 400px; font-weight: bold; color: white;">
    #                 <img src=www/logging.png height=200px class = circimg alt=old sawmill picture/img></center>
    #                        <p>(text to appear at the bottom left of the image)</p>
    #                        </div>'))),
    # 
    # fluidRow(column(width = 12, 
    #                 h2("Stories"),
    #                 p("The reasons for planting trees looks different all over our region. Explore some stories here.")),
    #          br()),
    # 
    #          HTML("<center><h3>Logging</h3></center>"),
    # fluidRow(column(width = 4,
    #                 HTML("<center><img src='www/logging.png' height='200px'  alt='old sawmill picture'/img></center>")),
    #          column(width = 8,
    #          HTML(paste0("<p>Over the last 200 years, our region has experienced profound land cover changes. Commericial logging in Minnesota began in 1839 with the founding of a sawmill in Marine on St. Croix. This created a legacy of reduced tree cover which still persists today. Learn more about our region's widespread deforestation by <a href = 'https://www.mnhs.org/foresthistory/learn/logging' target = '_blank'>visiting the Minnesota Historical Society website</a> or <a href = 'https://www.youtube.com/watch?v=HhqP6ghXKaU&t=1s' target = '_blank'>watching a video about logging in Stillwater.</a></p>")))),
    #   br(),
    # 
    # HTML("<center><h3>Invasive species</h3></center>"),
    # fluidRow(column(width = 4,
    #                 HTML("<center><img src='www/eab.png' height='200px'  alt='picture of Emerald Ash borer'/img></center>")),
    #          column(width = 8,
    #                 HTML(paste0("<p>Spread of invasive species like the Emerald Ash borer (invasive insect) or Dutch elm disease (invasive pathogen) threaten trees and the benefits they provide. Costing millions of dollars to manage with results that often fail to save trees, such invasive species have left the region with new canopy gaps. Read a story about <a href = 'https://www.mprnews.org/story/2015/03/15/ash-borer' target = '_blank'>towns currently managing the Emerald Ash Borer</a> and look at a <a href = 'http://collections.mnhs.org/mnhistorymagazine/articles/65/v65i02p44-53.pdf' target = '_blank'>report of how Dutch elm devastated trees in Minneapolis and St. Paul.</a></p>")))),
    # br(),
    # 
    # HTML("<center><h3>Extreme weather</h3></center>"),
    # fluidRow(column(width = 4,
    #                 HTML("<center><img src='www/tornado.jpeg' height='200px'  alt='picture of Emerald Ash borer'/img></center>")),
    #          column(width = 8,
    #                 HTML(paste0("<p>Extreme weather events like tornados and straight line winds are natural occurances in our region, although climate change can intensify their severity. Trees are suceptible to damage by these storms, and revegetation after weather events has been a challenge faced <a href = 'https://sahanjournal.com/climate/northside-tornado-recovery/' target = '_blank'>in North Minneapolis</a> as well as other communities in our region.</a></p>")))),
    # br(),
    # fluidRow(
    #   column(12, align = "center",
    # leafletOutput(ns("storymap"), height = 600, width = 600)))#'80%')))#,
    
    # shiny::div(
    #   id = "stories",
    #   includeMarkdown(system.file("app/www/stories.md", package = "eva.app"))
    # )
  )
}

#' intro Server Function
#'
#' @noRd 
mod_stories_server <- function(input, output, session){
  ns <- session$ns

  output$storymap <- renderLeaflet({
  leaflet() %>%
    setView(
      lat = st_coordinates(map_centroid)[2], #44.963,
      lng = st_coordinates(map_centroid)[1], #-93.22,
      zoom = 9
    ) %>%
    addMapPane(name = "Stamen Watercolor", zIndex = 430) %>%
    addProviderTiles("Stamen.Watercolor",
                     group = "Stamen Watercolor"
    ) %>%
    addProviderTiles("Stamen.TonerLabels", 
                     options = leafletOptions(pane = "Stamen Watercolor"),
                     group = "Stamen Watercolor") %>%
  
    # addMarkers
    # addCircleMarkers
      addAwesomeMarkers(lng = -93.14,
               lat = 44.96,
               # icon =  list(iconUrl = "https://i0.wp.com/forecastpublicart.org/wp-content/uploads/2017/12/Screen-Shot-2017-12-01-at-12.25.06-PM.png?fit=359%2C321&ssl=1", 
               #              iconSize = c(75, 75)),
               # radius = 20,
               # opacity = 1,
               # fillOpacity = 1,
               # color = "green",
               icon = icon_community,
               popup = ("Local artist Seitu Jones' Frogtown Farm project aims to create more greenspace in the heart of St. Paul's least forested neighborhood. <a href = 'https://forecastpublicart.org/seitu-jones/'>Read more about the work here.</a><img src = 'www/jones.jpeg' height = 200>")) %>%
    
      addAwesomeMarkers(lng = -93.10,
               lat = 44.96,
               # icon =  list(iconUrl = "https://static.wixstatic.com/media/e9184c_d8ffad244f41497696123ec07b905e09~mv2.jpg/v1/fill/w_255,h_255,al_c,q_80,usm_0.66_1.00_0.01/e9184c_d8ffad244f41497696123ec07b905e09~mv2.webp", 
                            # iconSize = c(75, 75)),
               # radius = 20,
               # opacity = 1,
               # fillOpacity = 1,
               # color = "green",
               icon = icon_community,
               popup = ("Frogtown Green has been planting trees in gravel beds <a href = 'https://www.frogtowngreen.com/tree-frogs-pop-up-park
'>Read more about their tree planting.</a><img src = 'www/ftg.jpeg' height = 200>")) %>%
    
      addAwesomeMarkers(lng = -93.23,
               lat = 44.95,
               # icon =  list(iconUrl = "https://woodfromthehood.com/wp-content/uploads/2012/08/Rick-Cindy-Jonfinal.jpg", 
               #              iconSize = c(75, 75)),
               # radius = 20,
               # opacity = 1,
               # fillOpacity = 1,
               # color = "yellow",
               icon = icon_bus,
               popup = ("Salvaging dead and diseased urban trees can give them a new life as wood floors, tables, or other useful produts. <a href = 'https://www.youtube.com/watch?v=_UeVStNvy3s&t=2s'>Watch a movie trailer about urban wood milling.</a> or learn more about local business <a href = 'https://woodfromthehood.com/about/'> Wood From the Hood producing 'wood with a zipcode'</a><img src='www/wfh.jpeg' height = 200>.")) %>%
    
      addAwesomeMarkers(lng = -93.27,
               lat = 44.98,
               # icon =  list(iconUrl = "https://64.media.tumblr.com/7aa29c12a83a09147505fab75eb6128e/tumblr_nnqctohvjJ1qfkgweo6_1280.jpg", 
               #              iconSize = c(75, 75)),
               # radius = 20,
               # opacity = 1,
               # fillOpacity = 1,
               # color = "red",
               icon = icon_cost,
               popup = ("Green roofs help reduce energy costs and reduce stormwater runoff. While often unsuitable for trees, greening roofs is another tool to know about. <a href = 'https://hclib.tumblr.com/post/123827675417/the-minneapolis-central-library-green-roof'>Read more about the Minneapolis Central Library's green roof here.</a>")) %>%
    
      addAwesomeMarkers(lng = -92.89,
               lat = 45.01,
               # icon =  list(iconUrl = "https://img.apmcdn.org/45642433c5be3ea6e6deafe375c11d18ffd168cf/uncropped/52ee63-20181205-christmas-trees03.jpg", 
               #              iconSize = c(75, 75)),
               # radius = 20,
               # opacity = 1,
               # fillOpacity = 1,
               # color = "yellow",
               icon = icon_cost,
               popup = ("Planting trees is a business for some Minnesota locals. Watch a <a href = 'https://www.youtube.com/watch?v=8n3IprkT5SE'>video about a local Christmas tree farm.</a> or read an <a href = 'https://www.mprnews.org/story/2018/12/05/christmas-tree-sustainability-footprint-real-artificial'>article about their sustainability footprint</a>.<img src='www/xmas.jpeg' height=200>")) %>%
      
      addAwesomeMarkers(lng = -93.56,
                 lat = 44.93,
                 # icon =  list(iconUrl = "https://www.minnehahacreek.org/sites/minnehahacreek.org/files/ScreenShot_16%20Nov.%2013%2014.21.jpg", 
                 #              iconSize = c(75, 75)),
                 # radius = 20,
                 # opacity = 1,
                 # fillOpacity = 1,
                 # color = "blue",
                 icon = icon_ecosystem,
                 popup = ("Planting trees helped restore shoreline on Lake Minnetonka's Big Island. <a href = 'https://www.minnehahacreek.org/project/big-island-restoration'>Read the success story</a>.<img src='www/bi.jpeg' height = 200>")) %>%
      
      addAwesomeMarkers(lng = -93.07,
                 lat = 44.57,
                 # icon =  list(iconUrl = "https://bloximages.chicago2.vip.townnews.com/agupdate.com/content/tncms/assets/v3/editorial/5/42/542724c6-a836-11eb-871d-f3caf49d4fd0/60897f203056e.image.jpg?resize=1200%2C676", 
                 #              iconSize = c(75, 75)),
                 # radius = 20,
                 # opacity = 1,
                 # fillOpacity = 1,
                 # color = "blue",
                 icon = icon_cost,
                 popup = ("Windbreaks are cool and tie agricultural communities into the story, SWCD work etc. <a href = 'https://www.agupdate.com/iowafarmertoday/news/livestock/windbreaks-pay-off-in-brutal-february/article_2b6d02b2-a836-11eb-ac32-b392c3805937.html'>Article from Iowa</a>.<br><img src='www/wb.jpeg' height=100>")) %>%
      
      addAwesomeMarkers(lng = -93.35,
                 lat = 45.31,
                 # icon =  list(iconUrl = "https://www.remicksorchard.com/file/2015/01/RemicksOrchard_21-400x533.jpg", 
                 #              iconSize = c(75, 75)),
                 # radius = 20,
                 # opacity = 1,
                 # fillOpacity = 1,
                 # color = "blue",
                 icon = icon_bus,
                 popup = ("Trees also provide food. <a href = 'https://www.startribune.com/anoka-county-natives-defy-naysayers-to-open-apple-orchard/225065272/'>Read the story of Remick's Orchard</a>.<img src='www/ro.jpeg' height=200>")) %>%
      
      addAwesomeMarkers(lng = -92.94,
                 lat = 44.91,
                 # icon =  list(iconUrl = "http://www.urbangreenforpeople.com/uploads/2/0/7/0/20706756/editor/thumbnail-dsc-0035.jpg?1616373767", 
                 #              iconSize = c(75, 75)),
                 # radius = 20,
                 # opacity = 1,
                 # fillOpacity = 1,
                 # color = "blue",
                 icon = icon_community,
                 popup = ("<a href = 'http://www.urbangreenforpeople.com/heritagetrees.html'>Woodbury trees have cultural importance.</a><br><img src='www/woodbury.jpeg' height=200>"))
    
  }
  )
 
}
    
## To be copied in the UI
# mod_stories_ui("stories_ui_1")
    
## To be copied in the server
# callModule(mod_stories_server, "stories_ui_1")
 
