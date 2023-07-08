#' input_update_button UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_update_button_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("btnUpdate"), "Update Model", icon("refresh"), width = "100%",
                 class = "btn btn-primary")
  )
}

#' input_update_button Server Functions
#'
#' @noRd
mod_input_update_button_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    reactive(input$btnUpdate)
  })
}

## To be copied in the UI
# mod_input_update_button_ui("input_update_button_1")

## To be copied in the server
# mod_input_update_button_server("input_update_button_1")
