#' biodiversity UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_biodiversity_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    br(),
    p("The University of MN has several lists of reccommended trees for Minnesota. This is the list for Southeast MN, which includes the MetCouncil region. If a tree-species-picker is something that seems valuable, it would probably be worth giving some thought to the variables which are included (think climate change, invasive spps, estimted $ value after X years, if species are 'approved' by major cities, adding in non-natives but popular street trees like ginko). More information about what the variables mean and why you might want to filter based on them would likely also be important.  https://extension.umn.edu/tree-selection-and-care/recommended-trees-minnesota "),
    
    h3("Tree characteristics:"),
    fluidRow(
             column(width = 3,
                    sliderInput(ns("height"), ("Height (feet)"),
                                min = 10, max = 60,
                                value = c(10, 60),
                                step = 5,
                                dragRange = F,
                                ticks = F)),
             column(width = 3,
                    sliderInput(ns("width"), ("Width (feet)"),
                                min = 5, max = 50,
                                value = c(5, 50),
                                step = 5,
                                dragRange = F,
                                ticks = F)),
             column(width = 3,
                    checkboxGroupInput(ns("type"), label = ("Tree type"), 
                                       choices = list("Conifer", "Deciduous"),
                                       selected = c("Conifer", "Deciduous"))),
             column(width = 3,
             shinyWidgets::pickerInput(ns("shape"),
                                       width = '90%',
                                       label = shiny::HTML("Tree shape"),
                                       choices=list("Irregular", "Oval", "Pyramidal", "Round", "Vase"), 
                                       options = list(`actions-box` = TRUE, 
                                                      size = 10,
                                                      `selected-text-format` = "count > 1"), 
                                       multiple = T,
                                       selected = list("Irregular", "Oval", "Pyramidal", "Round", "Vase")))),
    
    h3("Location requirements and aesthetics"),
    fluidRow(column(width = 2,
                    checkboxGroupInput(ns("street"), label = ("Street use?"), 
                                       choices = list("Yes", "No", "Sometimes"),
                                       selected = c("Yes", "No", "Sometimes"))),
             column(width = 2,
                    checkboxGroupInput(ns("power"), label = ("Under utility lines?"), 
                                       choices = list("Yes", "No", "Sometimes"),
                                       selected = c("Yes", "No", "Sometimes"))),
             column(width = 2,
                    checkboxGroupInput(ns("shade"), label = ("Light requirement"), 
                                       choiceNames = list("Full sun", "Partial shade", "Shade tolerant"),
                                       choiceValues = c("No", "Partial", "Yes"),
                                       selected = c("No", "Partial", "Yes"))),#),
    # h3("Aesthetics"),
    
    # fluidRow(
             column(width = 2,
                    checkboxGroupInput(ns("flower"), label = ("Flowering?"), 
                                       choices = list("Yes", "No"),
                                       selected = c("Yes", "No"))),
             column(width = 2,
                    checkboxGroupInput(ns("fruit"), label = ("Fruiting?"), 
                                       choices = list("Yes", "No"),
                                       selected = c("Yes", "No"))#)#,
             # column(width = 4,
             #        checkboxGroupInput(ns("fall"), label = ("Fall color"), 
             #                           choices = list("Red", "Yellow or orange", "Any"),
             #                           selected = c("Red", "Yellow or orange", "Any")))
             )),
    
    br(), 
    br(),
    DT::dataTableOutput(ns('spptable')),
                    
    hr("What trees to plant and what outcomes to expect?"),
    p("Considering the 'what' to plant is the next logical step after identifying 'where' to plant trees. Reliable tree biodiversity data does not exist for the entire region, but we can incorporate what information we do have.")
 
  )
}
    
#' biodiversity Server Function
#'
#' @noRd 
mod_biodiversity_server <- function(input, output, session){
  ns <- session$ns
  
  spp <- readr::read_csv("./data/SEMinn-Deciduous-recommended - Sheet1.csv",
                         skip = 1,
                         col_types = cols()) %>%
    # rename(height = `Height (feet)`,
    #        width= `Width (feet)`) %>%
    mutate(`Height (feet)` = as.numeric(gsub("[+]", "", `Height (feet)`)),
           `Width (feet)` = as.numeric(gsub("[+]", "", `Width (feet)`)),
           `Street use?` = gsub("1", "", `Street use?`),
           `Notable Flower` = gsub("no", "No", `Notable Flower`),
           `Notable Fruit` = gsub("no", "No", `Notable Fruit`))
  
  
  
  
  make_table_vals <-  reactive({
    
    suitable_trees <- spp %>%
      filter(`Height (feet)` >= min(input$height),
             `Height (feet)` <= max(input$height),
             `Width (feet)` >= min(input$width),
             `Width (feet)` <= max(input$width),
             Type %in% input$type,
             Shape %in% input$shape,
             `Street use?` %in% input$street,
             `Under utility lines?` %in% input$power,
             `Shade Tolerance` %in% input$shade,
             `Notable Flower` %in% input$flower,
             `Notable Fruit` %in% input$fruit
             ) 
    
    return(suitable_trees)
  })
  
  output$spptable <- DT::renderDataTable({
    DT::datatable(make_table_vals(), 
                  options = list(scrollX = TRUE,
                                 autoWidth = TRUE,
                                 pageLength = 5,
                                 columnDefs = list(list(width = '100px', targets = c(1, 3, 4, 5, 6, 7, 9, 10, 11)),
                                                   list(width = '250px', targets = c(2)),
                                                   list(width = '600px', targets = c(12)))))
    

  })
}
    
## To be copied in the UI
# mod_biodiversity_ui("biodiversity_ui_1")
    
## To be copied in the server
# callModule(mod_biodiversity_server, "biodiversity_ui_1")
 
