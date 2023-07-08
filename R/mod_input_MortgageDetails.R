#' input_MortgageDetails UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_MortgageDetails_ui <- function(id, title = "Mortgage", label = "tab_mortgage"){
  ns <- NS(id)
  tabPanel(
    title = title,
    value = ns(label),
    br(),
    textInput(ns("txtPropertyValue"), "Property value (£): ", value = ui_config$txtPropertyValue), shinyBS::bsTooltip("txtPropertyValue",  "Enter the total value of the property being purchased","bottom"),
    fluidRow(
      column(width = 6,
             textInput(ns("txtDeposit"), "Deposit: ", value = ui_config$txtDeposit)
      ),
      column(width = 6,
             radioButtons(ns("rdoDepositFormat"), label = "Deposit entry format", choices = c("£", "%"), selected = ui_config$rdoDepositFormat, inline = T)
      )
    ),
    br(),
    textInput(ns("txtMortgageLength"), "Mortgage length (years): ", value = ui_config$txtMortgageLength)
  )
}

#' input_MortgageDetails Server Functions
#'
#' @noRd
#'
#' @importFrom scales number
mod_input_MortgageDetails_server <- function(id, btnUpdate){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    property_value <- eventReactive({btnUpdate()
      session$clientData}, {
        input$txtPropertyValue %>% clean_numeric_input(0)
      })

    deposit_radio <- reactive({
      input$rdoDepositFormat
    })

    observe({
      deposit_text <- input$txtDeposit %>% clean_numeric_input(0)
      prop_value = input$txtPropertyValue %>% clean_numeric_input(0)

      deposit_num <- if (input$rdoDepositFormat == "£" && deposit_text <= 100) {
        acc_max = 0
        deposit_text * prop_value / 100
      } else if (input$rdoDepositFormat == "%" && deposit_text > 100) {
        acc_max = 2
        100 * deposit_text / prop_value
      } else {
        acc_max = 2
        deposit_text

      }

      txt_pref <- c("%" = "", "£" = "£")[input$rdoDepositFormat]
      txt_suff <- c("%" = "%", "£" = "")[input$rdoDepositFormat]

      acc <- min(count_decimals(deposit_num), acc_max, na.rm = T)

      updateTextInput(session, "txtDeposit", "Deposit: ",
                      value = scales::number(deposit_num, prefix = txt_pref, suffix = txt_suff, big.mark = "", accuracy = 10 ^ -acc))
    })

    deposit <- reactive({

      deposit_raw <- input$txtDeposit %>%
        clean_numeric_input(0)

      if (deposit_radio() == "%") {
        deposit_raw <- deposit_raw %>%
          {. / 100} %>%
          {. * input$txtPropertyValue %>% clean_numeric_input(0)}

      }

      deposit_raw
    })

    deposit_out <- eventReactive({btnUpdate()
      session$clientData}, {
        deposit()
      })

    mortgage_length <- reactive({
      input$txtMortgageLength %>% clean_numeric_input(25, min = 1, max = 100)
    })

    loan_principle = eventReactive({btnUpdate()
      session$clientData}, {
        property_value() - deposit()
      })

    return(
      list(
        property_value = property_value,
        loan_principle = loan_principle,
        deposit = deposit_out,
        mortgage_length = mortgage_length
      )
    )
  })
}
