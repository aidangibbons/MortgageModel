#' output_MonthlyHeadroom UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_output_MonthlyHeadroom_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' output_MonthlyHeadroom Server Functions
#'
#' @noRd 
mod_output_MonthlyHeadroom_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_output_MonthlyHeadroom_ui("output_MonthlyHeadroom_1")
    
## To be copied in the server
# mod_output_MonthlyHeadroom_server("output_MonthlyHeadroom_1")
