#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  btnUpdate <- mod_input_update_button_server("input_button")


  input_mortgage <- mod_input_MortgageDetails_server("input_mortgagedetails", btnUpdate)
  input_interestrates <- mod_input_InterestRates_server("input_interestrates", btnUpdate)
  input_housingposition <- mod_input_HousingPosition_server("input_housingposition", btnUpdate, input_mortgage$property_value)
  input_finances <- mod_input_Finances_server("input_finances", btnUpdate,
                                              existing_property_value = input_housingposition$current_property_value)
  input_costs <- mod_input_Costs_server("input_costs", btnUpdate, stamp_duty = input_housingposition$stamp_duty, deposit = input_mortgage$deposit,
                                        total_liquid = input_finances$total_liquid, target_emergency_fund = input_finances$target_emergency_fund)

  payment_df <- mod_model_calculation_server("calculations", btnUpdate, input_mortgage, input_interestrates, input_housingposition, input_finances, input_costs)

  mod_output_SummaryText_server("output_summarytext", payment_df, input_mortgage, input_interestrates, input_housingposition, input_finances, input_costs
                                )

  mod_output_TotalAssets_server("output_assets",
                                payment_df,
                                deposit = input_mortgage$deposit,
                                mortgage_length = input_mortgage$mortgage_length,
                                initial_term_length = input_interestrates$initial_term_length)

  mod_output_MonthlyHeadroom_server("output_headroom",
                                    payment_df,
                                    initial_term_length = input_interestrates$initial_term_length,
                                    monthly_takehome = input_finances$monthly_takehome,
                                    rental_income = input_finances$rental_income,
                                    total_outgoings = input_finances$total_outgoings)

  mod_output_PaymentSplit_server("output_paymentsplit", payment_df)

}
