#' input_HousingPosition UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_HousingPosition_ui <- function(id, title = "Housing Position", label = "tab_housingposition"){
  ns <- NS(id)

  tabPanel(
    title = title,
    label = ns(label),
    br(),
    checkboxInput(ns("chkFTB"), "First-time buyer?", value = ui_config$chkFTB),
    checkboxInput(ns("chkSellingHouse"), "Selling current house?", value = ui_config$chkSellingHouse),
    textInput(ns("txtCurrentHouseValue"), "Current house sale price (£)", value = ui_config$txtCurrentHouseValue)
  )
}

#' input_HousingPosition Server Functions
#'
#' @noRd
#'
#' @importFrom dplyr case_when
mod_input_HousingPosition_server <- function(id, btnUpdate, property_value){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    stamp_duty = eventReactive({btnUpdate()
      session$clientData}, {
      prop_val <- property_value()

      sdTier2 <- 0.05 * (925000 - 250000)
      sdTier3 <- 0.1 * (1500000 - 925000)

      sd <- case_when(
        input$chkFTB & prop_val <= 425000 ~ 0,
        input$chkFTB & prop_val <= 625000 ~ 0.05 * (prop_val - 425000),
        prop_val <= 250000 ~ 0,
        prop_val <= 925000 ~ 0.05 * (prop_val - 250000),
        prop_val <= 1500000 ~ sdTier2 + 0.1 * (prop_val - 925000),
        TRUE ~ sdTier2 + sdTier3 + 0.12 * (prop_val - 1500000)
      )

      sd
    })

    current_property_value <- reactive({
      if (input$chkSellingHouse) {
        input$txtCurrentHouseValue %>% clean_numeric_input(0)
      } else {
        0
      }
    })

    return(list(
      stamp_duty = stamp_duty,
      current_property_value = current_property_value
    ))

  })
}

## To be copied in the UI
# mod_input_HousingPosition_ui("input_HousingPosition_1")

## To be copied in the server
# mod_input_HousingPosition_server("input_HousingPosition_1")
