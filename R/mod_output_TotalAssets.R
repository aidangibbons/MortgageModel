#' output_TotalAssets UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_output_TotalAssets_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' output_TotalAssets Server Functions
#'
#' @noRd 
mod_output_TotalAssets_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_output_TotalAssets_ui("output_TotalAssets_1")
    
## To be copied in the server
# mod_output_TotalAssets_server("output_TotalAssets_1")
