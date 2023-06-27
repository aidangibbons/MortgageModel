#' output_PaymentSplit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_output_PaymentSplit_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' output_PaymentSplit Server Functions
#'
#' @noRd 
mod_output_PaymentSplit_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_output_PaymentSplit_ui("output_PaymentSplit_1")
    
## To be copied in the server
# mod_output_PaymentSplit_server("output_PaymentSplit_1")
