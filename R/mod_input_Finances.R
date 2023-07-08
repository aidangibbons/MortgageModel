#' input_Finances UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_Finances_ui <- function(id, title = "Finances", label = "tab_finances"){
  ns <- NS(id)
  tabPanel(
    title = title,
    label = ns(label),
    br(),
    textInput(ns("txtTotalLiquid"), "Total initial sum available (including deposit) (£): ", value = ui_config$txtTotalLiquid),
    hr2(),
    p("Available money ('total available for mortgage monthly' + new rental income) will be put towards mortgage repayments, then an emergency fund, and then split between savings and overpayments as required"),
    textInput(ns("txtTotalRentSavings"), "Total available for mortgage monthly (£/m): ", value = ui_config$txtTotalRentSavings), # TODO change ID name
    textInput(ns("txtTotalOutgoings"), "Non-mortgage monthly-spend (£/m): ", value = ui_config$txtTotalOutgoings),
    textInput(ns("txtRent"), "Rental Income (£/m): ", value = ui_config$txtRent), # TODO
    textInput(ns("txtRentalTax"), "Rental tax (applied at rent above £7500/yr) (%): ", value = ui_config$txtRentalTax), # TODO
    hr2(),
    textInput(ns("txtEmergencyFund"), "Emergency fund target (£): ", value = ui_config$txtEmergencyFund), # TODO
    # textInput(ns("txtSavingsTarget"), "Savings target (£): ", value = ui_config$txtSavingsTarget), # TODO
    p("The following checkbox will allow for overpayments."),
    p("This works by splitting all 'spare' income in a month, into overpaying the mortgage, or into savings, as specified."),
    fluidRow(
      column(width = 6, # TODO
             # TODO add tooltip
             textInput(ns("txtSavingsOverpaymentSplit"), "Proportion of spare income for savings (vs overpayment) (%): ", value = ui_config$txtSavingsOverpaymentSplit)),
      # textInput(ns("txtOverpayment"), label = "Regular overpayment (£ per month): ", value = ui_config$txtOverpayment)), # TODO implement
      column(width = 6,
             checkboxInput(ns("chkOverpaymentToggle"), label = "Enable auto-overpayment?", value = ui_config$chkOverpaymentToggle))#, # TODO implement
      # column(width = 12,
      #        textInput(ns("txtOverpaymentLimit"), label = "Overpayment limit (% of remaining mortgage/yr)", value = ui_config$txtOverpaymentLimit) # TODO implement
      # )
    )
  )
}

#' input_Finances Server Functions
#'
#' @noRd
mod_input_Finances_server <- function(id, btnUpdate, existing_property_value){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # TODO continue converting reactive to eventReactive(btnUpdate()... in this and _Costs.R

    available_liquid = eventReactive({btnUpdate()
      session$clientData}, {input$txtTotalLiquid %>% clean_numeric_input})
    monthly_rent_and_savings = eventReactive({btnUpdate()
      session$clientData}, {input$txtTotalRentSavings %>% clean_numeric_input})
    total_outgoings = reactive({input$txtTotalOutgoings %>% clean_numeric_input})
    rental_income_raw = reactive({input$txtRent %>% clean_numeric_input})
    rental_tax_pct = reactive({input$txtRentalTax %>% clean_numeric_input}) # %>% {. / 100}
    target_emergency_fund = reactive({input$txtEmergencyFund %>% clean_numeric_input})
    target_savings = reactive({100000000})

    monthly_takehome = reactive({monthly_rent_and_savings() + total_outgoings()})

    # overpayment_chosen = reactive({input$txtOverpayment %>% clean_numeric_input})
    monthly_capacity = reactive({input$txtTotalRentSavings %>% clean_numeric_input}) # %>% {. / 12}

    savings_overpayment_split = eventReactive({btnUpdate()
      session$clientData}, {
        if (input$chkOverpaymentToggle) {
          input$txtSavingsOverpaymentSplit %>% clean_numeric_input(50, min = 0, max = 100)
        } else {
          100
        }
      })


    # rental_income = reactive({
    #   rental_income_raw() -
    #     (max(0, rental_income_raw() - 7500 / 12) * rental_tax_pct() / 100)
    # })

    rental_income = reactive({
      calculate_rental_income_pm(rental_income_raw(), rental_tax_pct(), 7500)
    })

    # observe({
    #   if (input$chkOverpaymentToggle) {
    #     updateTextInput(session, "txtOverpayment", "Regular overpayment (£ per month):",
    #                     value = "£0")
    #   } else {
    #     updateTextInput(session, "txtOverpayment", "Regular overpayment (£ per month):",
    #                     value = "£100")
    #   }
    # })

    # observe({
    #   if (input$chkOverpaymentToggle) {
    #     updateTextInput(session, "txtOverpayment", "Regular overpayment (£ per month):",
    #                     value = "£0")
    #   } else {
    #     updateTextInput(session, "txtOverpayment", "Regular overpayment (£ per month):",
    #                     value = "£100")
    #   }
    # })


    total_liquid = reactive({
      available_liquid() + existing_property_value()
    })


    # TODO make mortgage function either take
    #  chosen overpayment, or automated one
    overpayment_savings_calculated <- reactive({
      overpayment_chosen = overpayment_chosen()
      monthly_capacity = monthly_capacity()
      savings_overpayment_split = savings_overpayment_split()
      if (input$chkOverpaymentToggle) {
        savings_split = savings_overpayment_split
        calculated_overpayment = monthly_capacity * (100 - savings_split) / 100
      } else {
        calculated_overpayment = overpayment_chosen
      }

      return(list(overpayment = calculated_overpayment,
                  savings = monthly_capacity - calculated_overpayment))


    })


    return(
      list(
        available_liquid = available_liquid,
        total_liquid = total_liquid,
        monthly_rent_and_savings = monthly_rent_and_savings,
        total_outgoings = total_outgoings,
        monthly_takehome = monthly_takehome,
        rental_income = rental_income,
        target_emergency_fund = target_emergency_fund,
        target_savings = target_savings,
        # overpayment_chosen = overpayment_chosen,
        monthly_capacity = monthly_capacity,
        savings_overpayment_split = savings_overpayment_split,
        overpayment_savings_calculated = overpayment_savings_calculated
      )
    )

  })
}

calculate_rental_income_pm <- function(monthly_rental_income, rental_tax_percentage, rental_income_limit = 7500) {
  monthly_rental_income -
    (max(0, monthly_rental_income - rental_income_limit / 12) * rental_tax_percentage / 100)
} # TODO test this for sample cases


## To be copied in the UI
# mod_input_Finances_ui("input_Finances_1")

## To be copied in the server
# mod_input_Finances_server("input_Finances_1")
