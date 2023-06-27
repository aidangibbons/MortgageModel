#' input_HousingPosition UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_input_HousingPosition_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' input_HousingPosition Server Functions
#'
#' @noRd 
mod_input_HousingPosition_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_input_HousingPosition_ui("input_HousingPosition_1")
    
## To be copied in the server
# mod_input_HousingPosition_server("input_HousingPosition_1")
