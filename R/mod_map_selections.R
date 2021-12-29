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
    radioButtons(ns("preset"),
      HTML("<h2><section style='font-size:20pt'>Prioritization layer</h2></section><p><section style='font-weight: normal;'>Trees intersect with regional issues and priorities. Select a priority layer to understand the overlap.</section></p>"),
      choices = c(
        "Climate change",
        "Conservation",
        "Environmental justice",
        "Public health",
        "Custom"
      ), inline = T,
      selected = "Environmental justice"
    ) %>%
      shinyhelper::helper(
        type = "markdown", content = "PresetHelp", size = "l"
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
        # choicesOpt = list(
        #   subtext = paste0(dplyr::filter(metadata, type == "environment") %>% .$nicer_interp)
        # ),
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
mod_map_selections_server <- function(input, output, session,
                                      # preset_selections,
                                      current_tab) {
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
