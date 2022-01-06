#' map_selections UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
#'
mod_map_selections_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # fluidRow(
      radioButtons(ns("preset"),
      HTML("<h2><section style='font-size:20pt'>Priority layer</h2></section><p><section style='font-weight: normal;'>Trees intersect with regional issues and priorities. Use a preset or create a custom layer to understand the overlap. </section></p>"),
      choiceNames = list(
        # HTML("<a>Climate change</a>"),
        # HTML("<a>Conservation</a>"),
        # HTML("<a>Environmental justice</a>"),
        # HTML("<a>Public health</a>"),
        # HTML("<a>Custom</a>")
        "Climate change", 
        "Conservation",
        "Environmental justice",
        "Public health",
        "Custom"
      ),
      choiceValues = c(
        "Climate change",
        "Conservation",
        "Environmental justice",
        "Public health",
        "Custom"
      ), inline = T,
      selected = "Environmental justice"
      # selected = "Custom"
    # ),
    # shinyBS::bsModal(ns("testmodal"), title = "Test Modal", trigger = "Climate change",
    #                  h5("Data Guidelines"),
    #                  tags$ol(
    #                    tags$li("Must have Resp_ID as the first column, occasion_ID as second and dependent variable as the third"),
    #                    tags$li("Must have no missing value in any fields")
    #                  ), easyClose = TRUE, footer = NULL),
    # radioTooltip(ns("preset"), choice = "Environmental justice", title = "The natural and built environments intersect with income, race, and ethnicity. This preset identifies areas and people facing disproportionately negative consequences of environmental decisions.", placement = "bottom", trigger = "hover"),
    # radioTooltip(ns("preset"), choice = "Conservation", title = "Reducing tree canopy loss will be critical to meet carbon emission reduction goals and conserve biodiversity across taxa. This preset identifies areas with the region’s highest stock of existing trees.", placement = "bottom", trigger = "hover"),
    # radioTooltip(ns("preset"), choice = "Custom", title = "Select this option to customize the prioritization variables.", placement = "bottom", trigger = "hover"),
    # radioTooltip(ns("preset"), choice = "Climate change", title = "Trees can mitigate some consequences of climate change by cooling land surface temperatures and reducing flooding. Use this preset to identify areas most at risk from climate change.", placement = "bottom", trigger = "hover"),
    # radioTooltip(ns("preset"), choice = "Public health", title = "Trees improve air quality and cool land surface temperatures leading to better health outcomes, particularly for sensitive populations. Identify areas where trees could most improve health outcomes.", placement = "bottom", trigger = "hover"),
    )  %>%
      shinyhelper::helper(
        type = "markdown", content = "PresetHelp", size = "m"
        # )
      ),
    conditionalPanel(
      ns = ns,
      condition = "input.preset == 'Custom'", # && input.onoff == 'On'",

      shinyWidgets::pickerInput(ns("peopleInput"),
        label = shiny::HTML(paste0("<h4><span style='font-size:14pt'>Demographics</span></h4>")),
        choices = dplyr::filter(metadata, type == "people") %>% .$name,
        # choicesOpt = list(
        #   subtext = paste0(filter(metadata, type == "people") %>% .$niceinterp,
        #                    " values have higher priority scores")),
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 1"
        ),
        multiple = T,
        selected = NULL # filter(metadata, type == "people")[1, 2]
      )
    ),
    conditionalPanel(
      ns = ns,
      condition = "input.preset == 'Custom'", # && input.onoff == 'On'",

      shinyWidgets::pickerInput(ns("placeInput"),
        label = shiny::HTML(paste0("<h4><span style='font-size:14pt'>Environment & Climate</span></h4>")),
        choices = dplyr::filter(metadata, type == "environment") %>% .$name,
        choicesOpt = list(
          subtext = paste0(dplyr::filter(metadata, type == "environment") %>% .$nicer_interp)
        ),
        # choicesOpt = list(
        #   subtext = paste0(filter(metadata, type == "environment") %>% .$niceinterp,
        #                    " values have higher priority scores")),
        options = list(
          `actions-box` = TRUE,
          size = 20,
          `selected-text-format` = "count > 1"
        ),
        multiple = T,
        selected = dplyr::filter(metadata, type == "environment")[7, 2]
      )
    ),
    conditionalPanel(
      ns = ns,
      condition = "input.preset == 'Custom'", # && input.onoff == 'On'",

      shinyWidgets::pickerInput(ns("healthInput"),
        label = shiny::HTML(paste0("<h4><span style='font-size:14pt'>Health</span></h4>")),
        choices = dplyr::filter(metadata, type == "health") %>% .$name,
        # choicesOpt = list(
        #   subtext = paste0(filter(metadata, type == "health") %>% .$niceinterp,
        #                     " values have higher priority scores")),
        options = list(
          `actions-box` = TRUE,
          size = 20,
          `selected-text-format` = "count > 1"
        ),
        multiple = T,
        selected = NULL
      )
    ),
    conditionalPanel(
      ns = ns,
      condition = "input.preset == 'Custom'", # && input.onoff == 'On'",

      shinyWidgets::pickerInput(ns("dollarInput"),
        label = shiny::HTML(paste0("<h4><span style='font-size:14pt'>Socioeconomics</span></h4>")),
        choices = dplyr::filter(metadata, type == "dollar") %>% .$name,
        # choicesOpt = list(
        #   subtext = paste0(filter(metadata, type == "dollar") %>% .$niceinterp,
        #                    " values have higher priority scores")),
        options = list(
          `actions-box` = TRUE,
          size = 20,
          `selected-text-format` = "count > 1"
        ),
        multiple = T,
        selected = NULL
      )
    )
  )
}

#' map_selections Server Function
#'
#' @noRd
mod_map_selections_server <- function(input, output, session#,
                                      # preset_selections,
                                      # current_tab
                                      ) {
  ns <- session$ns

  input_values <- reactiveValues() # start with an empty reactiveValues object.

  observe({
    input_values$allInputs <- as_tibble(input$peopleInput) %>%
      rbind(as_tibble(input$placeInput)) %>%
      rbind(as_tibble(input$healthInput)) %>%
      rbind(as_tibble(input$dollarInput))

    input_values$preset <- input$preset
  })
  return(input_values)
}



## To be copied in the UI
# mod_map_selections_ui("map_selections_ui_1")

## To be copied in the server
# callModule(mod_map_selections_server, "map_selections_ui_1")
