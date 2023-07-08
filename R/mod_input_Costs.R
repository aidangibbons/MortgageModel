#' input_Costs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_Costs_ui <- function(id, title = "Costs", label = "tab_costs"){
  ns <- NS(id)
  tabPanel(
    title = title,
    value = ns(label),
    h3("Upfront costs: "),
    textInput(ns("txtSurveyFee"), "Survey Fee (£): ", value = ui_config$txtSurveyFee),
    textInput(ns("txtSolicitorFee"), "Solicitor Fee (£): ", value = ui_config$txtSolicitorFee),
    textInput(ns("txtMortgageFee"), "Mortgage fee (this will be added to mortgage total) (£): ", value = ui_config$txtMortgageFee),
    textInput(ns("txtMortgageAdvisorFee"), "Mortgage advisor fee (£): ", value = ui_config$txtMortgageAdvisorFee),
    textInput(ns("txtMovingCosts"), "Moving Costs (£): ", value = ui_config$txtMovingCosts),
    textInput(ns("txtLandRegistry"), "Land Registry fee (£)", value = ui_config$txtLandRegistry),
    textInput(ns("txtAdditionalCosts"), "Additional upfront costs (£): ", value = ui_config$txtAdditionalCosts),
    br(),
    h3("Ongoing costs:"),
    textInput(ns("txtGroundRent"), label = "Ground rent (£/yr): ", value = ui_config$txtGroundRent),
    textInput(ns("txtServiceCharge"), label = "Service Charge (£/yr): ", value = ui_config$txtServiceCharge),
    textInput(ns("txtInsurance"), "Homeowners insurance (£/yr): ", value = ui_config$txtInsurance),
    textInput(ns("txtMaintenance"), "Annual property maintenance (£/yr): ", value = ui_config$txtMaintenance)
  )
}

#' input_Costs Server Functions
#'
#' @noRd
mod_input_Costs_server <- function(id, btnUpdate, stamp_duty, deposit, total_liquid, target_emergency_fund){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    survey_fee = reactive({input$txtSurveyFee %>% clean_numeric_input})
    solicitor_fee = reactive({input$txtSolicitorFee %>% clean_numeric_input})
    mortgage_fee = reactive({input$txtMortgageFee %>% clean_numeric_input})
    mortgage_advisor_fee = reactive({input$txtMortgageAdvisorFee %>% clean_numeric_input})
    moving_costs = reactive({input$txtMovingCosts %>% clean_numeric_input})
    land_registry = reactive({input$txtLandRegistry %>% clean_numeric_input})
    additional_upfront_costs = reactive({input$txtAdditionalCosts %>% clean_numeric_input})

    ground_rent_pa = reactive({input$txtGroundRent %>% clean_numeric_input})
    service_charge_pa = reactive({input$txtServiceCharge %>% clean_numeric_input})
    insurance_pa = reactive({input$txtInsurance %>% clean_numeric_input})
    maintenance_pa = reactive({input$txtMaintenance %>% clean_numeric_input})

    initial_costs_sum <- reactive({
      sum(
        stamp_duty(),
        survey_fee(),
        solicitor_fee(),
        mortgage_fee(),
        mortgage_advisor_fee(),
        moving_costs(),
        land_registry(),
        additional_upfront_costs()
      )

    })

    initial_costs <- eventReactive({btnUpdate()
      session$clientData}, {
      calculate_initial_costs(initial_costs_sum(),
                              deposit = deposit(),
                              total_liquid = total_liquid(),
                              emergency_fund_target = target_emergency_fund())
    })

    regular_payments = reactive({
      tibble::tibble("ground_rent" = ground_rent_pa(),
             "service_charge" = service_charge_pa(),
             "insurance" = insurance_pa(),
             "maintenance" = maintenance_pa(),
             "council_tax" = 0, # TODO
             "bills" = 0) %>%
        select(where(is.numeric)) %>%
        select(where(~sum(.) != 0))
    })



    return(
      list(
        initial_costs = initial_costs,
        regular_payments = regular_payments
      )
    )


  })
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param initial_costs_sum PARAM_DESCRIPTION, Default: 0
#' @param deposit PARAM_DESCRIPTION, Default: 0
#' @param total_liquid PARAM_DESCRIPTION, Default: 0
#' @param emergency_fund_target PARAM_DESCRIPTION, Default: 0
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname calculate_initial_costs
#' @export
calculate_initial_costs <- function (initial_costs_sum = 0,
                                     deposit = 0,
                                     total_liquid = 0,
                                     emergency_fund_target = 0) {


  total_initial_costs = initial_costs_sum + deposit
  total_initial_lump_sum = total_liquid

  if (total_initial_lump_sum < total_initial_costs) {
    stop("Initial costs exceed available funds. Increase available funds, or reduce upfront costs.")
  }

  remaining_funds = total_initial_lump_sum - total_initial_costs

  initial_emergency_fund = min(remaining_funds, emergency_fund_target)

  initial_sum_remaining = max(remaining_funds - emergency_fund_target,
                              0)

  c("initial_emergency_fund" = initial_emergency_fund,
    "initial_sum_remaining" = initial_sum_remaining,
    "total_initial_costs" = total_initial_costs,
    "total_initial_lump_sum" = total_initial_lump_sum)

}
