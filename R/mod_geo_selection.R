#' geo_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_geo_selection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # waiter::useWaitress(),
    # shinybrowser::detect(),
    
HTML("<h2><section style='font-size:20pt'>Geography</h2>"),
    radioButtons(
      ns("geo"),
      # h4("Report area"),
      label = HTML("</section><p><section style='font-weight: normal;' class='d-none d-lg-block'>Make a selection to create a custom report. <strong>Scoll down to read and download the report.</strong></section></p>"),
      # choices = c(
      #   "Cities and townships" = "ctus",
      #   "Neighborhoods (Minneapolis and St.Paul only)" = "nhood",
      #   "A Census block group" = "blockgroups"
      # ), # multiple = F,
      choiceNames = list("Cities and townships", 
                         HTML("<section class='d-block d-lg-none'>Neighborhoods</section>
                              <section class='d-none d-lg-block'>Neighborhoods</section>"), # (Minneapolis and St.Paul only)</section>"), #desktop
                         "Census block group"),
      choiceValues = list("ctus", "nhood", "blockgroups"),
      selected = "ctus",
      # inline = (shinybrowser::get_device() == "Mobile")
    ),
    # hr(),

    # uiOutput(ns("geodropdowns"))
fluidRow(column(width = 6,
                conditionalPanel(
      ns = ns,
      condition = "input.geo == 'ctus'",
      shinyWidgets::pickerInput(ns("cityInput"),
        label = shiny::HTML(paste0("<h3><span style='font-size:14pt'>Pick a city or township</span></h3>")),
        choices = ctu_list$GEO_NAME,
        options = list(
          title = "Pick a city or township", size = 10,
          `live-search` = TRUE
        ),
        multiple = F,
        selected = ui_params$set[ui_params$param == "cityselected"]
      )
    ),
    conditionalPanel(
      ns = ns,
      condition = "input.geo == 'nhood'",
      shinyWidgets::pickerInput(ns("nhoodInput"),
        label = shiny::HTML(paste0("<h4><span style='font-size:14pt'>Pick a neighborhood</span></h4>")),
        choices = nhood_ui,
        options = list(
          title = NULL, size = 10,
          `live-search` = TRUE
        ),
        multiple = F,
        selected = ui_params$set[ui_params$param == "nhoodselected"]
      )
    ),
    conditionalPanel(
      ns = ns,
      condition = "input.geo == 'blockgroups'",
      HTML("Please click on an area within the map at right.")
    )
),
# column(width = 6,
#        radioButtons(
#          ns("mapfilter"),
#          # h4("Report area"),
#          label = HTML("<h4><span style='font-size:14pt'>Map priority scores</span></h4>"),
#          choices = c(
#            "All scores" = "nofilter",
#            "Above 5" = "above5",
#            "Above 6" = "above6",
#            "Above 7" = "above7"
#            # "All priority scores" = "nofilter",
#            # "Priority scores above 5" = "above5"
#            ), # multiple = F,
#          selected = "nofilter", inline = T
#        ))
column(width = 6,
       div(style = "color:'green';",
           shinyWidgets::radioGroupButtons(
         ns("mapfilter"),
         # h4("Report area"),
         label = HTML("<h4><span style='font-size:14pt'>Map priority scores</span></h4>"),
         choices = c(
           "All scores" = "nofilter",
           ">4" = "above4",
           ">5" = "above5",
           ">6" = "above6",
           ">7" = "above7"
           # "All priority scores" = "nofilter",
           # "Priority scores above 5" = "above5"
         ), # multiple = F,
         selected = "nofilter"
       )))
))
}

#' geo_selection Server Functions
#'
#' @noRd
mod_geo_selection_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    input_values <- reactiveValues()
    observe({
      input_values$selected_geo <- input$geo
      input_values$mapfilter <- input$mapfilter
      input_values$selected_area <- if (input$geo == "ctus") {
        input$cityInput
      } else if (input$geo == "nhood") {
        input$nhoodInput
      } else {
        ""
      }
    })
    return(input_values)
  })
}

## To be copied in the UI
# mod_geo_selection_ui("geo_selection_ui_1")

## To be copied in the server
# mod_geo_selection_server("geo_selection_ui_1")
