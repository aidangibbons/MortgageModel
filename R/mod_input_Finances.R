#' input_Finances UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_input_Finances_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' input_Finances Server Functions
#'
#' @noRd 
mod_input_Finances_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_input_Finances_ui("input_Finances_1")
    
## To be copied in the server
# mod_input_Finances_server("input_Finances_1")
