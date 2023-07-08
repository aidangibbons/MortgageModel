require(shinyBS)

#' input_panel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_panel_ui <- function(id){
  ns <- NS(id)

  tagList(
    tabsetPanel(
      mod_input_MortgageDetails_ui("input_mortgagedetails", "Mortgage", "tab_mortgage"),
      mod_input_InterestRates_ui("input_interestrates", "Interest Rates", "tab_interestrates"),
      mod_input_HousingPosition_ui("input_housingposition", "Housing Position", "tab_housingposition"),
      mod_input_Finances_ui("input_finances", "Finances", "tab_finances"),
      mod_input_Costs_ui("input_costs", "Costs", "tab_costs")
    )

  )
}

#' input_panel Server Functions
#'
#' @noRd
# mod_input_panel_server <- function(id){
#   moduleServer(id, function(input, output, session){
#     ns <- session$ns
#
#     mod_input_MortgageDetails_ui("mortgagedetails")
#     input_tab_mortgage_server(input, output, session)
#     input_tab_interestrates_server(input, output, session)
#     input_tab_housingposition_server(input, output, session)
#     input_tab_finances_server(input, output, session)
#     input_tab_costs_server(input, output, session)
#
#
#
#   })
# }

## To be copied in the UI
# mod_input_panel_ui("input_panel_1")

## To be copied in the server
# mod_input_panel_server("input_panel_1")
