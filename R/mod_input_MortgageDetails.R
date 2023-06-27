#' input_MortgageDetails UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_input_MortgageDetails_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' input_MortgageDetails Server Functions
#'
#' @noRd 
mod_input_MortgageDetails_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_input_MortgageDetails_ui("input_MortgageDetails_1")
    
## To be copied in the server
# mod_input_MortgageDetails_server("input_MortgageDetails_1")
