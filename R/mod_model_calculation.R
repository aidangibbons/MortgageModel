#' model_calculation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinycssloaders withSpinner
mod_model_calculation_ui <- function(id, title = "Table of Expenses", label = "tab_tables"){
  ns <- NS(id)
  tabPanel(
    title = title,
    value = ns(label),
    tabsetPanel(
      tabPanel(
        title = "Annual",
        label = "annual",
        DT::DTOutput(ns("paymentsAnnual")) %>% withSpinner(color="#0dc5c1")
      ),
      tabPanel(
        title = "Monthly",
        label = "monthly",
        DT::DTOutput(ns("paymentsMonthly"))
      )
    )
  )
}

#' model_calculation Server Functions
#'
#' @noRd
#'
#' @import dplyr
mod_model_calculation_server <- function(id, btnUpdate,  inp_mortgage, inp_interest, inp_housing, inp_finances, inp_costs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    payment_df <- eventReactive({btnUpdate()
      session$clientData}, {

        calculate_complex_payments(mortgage_length = inp_mortgage$mortgage_length(),
                                   loan_principle = inp_mortgage$loan_principle(),
                                   deposit = inp_mortgage$deposit(),
                                   interest_vec = inp_interest$rate_vec(),
                                   regular_payments = inp_costs$regular_payments(),
                                   monthly_capacity = inp_finances$monthly_capacity(),
                                   # overpayment_chosen = overpayment_chosen(),
                                   rental_income = inp_finances$rental_income(),
                                   initial_sum_remaining = inp_costs$initial_costs()["initial_sum_remaining"],
                                   initial_emergency_fund = inp_costs$initial_costs()["initial_emergency_fund"],
                                   emergency_target = inp_finances$target_emergency_fund(),
                                   savings_target = inp_finances$target_savings(),
                                   saving_OP_pct = inp_finances$savings_overpayment_split(),
                                   monthly_outgoings = inp_finances$total_outgoings()
        )
      })


    output$paymentsAnnual <- DT::renderDT({

      payment_df() %>%
        mutate(year = floor(month / 12)) %>%
        select(-month) %>%
        group_by(year) %>%
        summarise(
          across(c(balance_remaining), ~round(last(.))),
          across(tidyselect::any_of(c("monthly_payment", "total_monthly_payment", "equity_paid_off", "savings_added", "emergency_fund_added",
                                             "interest_split", "equity_split", "overpayment", names(inp_costs$regular_payments()))),
                        ~round(sum(., na.rm = T)))
        ) %>%
        mutate(across(c(savings_added, emergency_fund_added), cumsum)) %>%
        rename_with(.cols = tidyselect::contains("month"),
                           ~stringr::str_replace(., "monthly", "annual")) %>%
        rename_with(.cols = tidyselect::contains("_added"),
                           ~stringr::str_remove(., "_added")) %>%
        select(-total_annual_payment) %>%
        mutate(year = year + 1) %>%
        rename_with(~stringr::str_to_title(stringr::str_replace_all(., "_", " "))) %>%
        DT::datatable(rownames = F)
    })

    output$paymentsMonthly <- DT::renderDT({
      payment_df() %>%
        mutate(across(tidyselect::everything(), round, 2)) %>%
        rename_with(~stringr::str_to_title(stringr::str_replace_all(., "_", " "))) %>%
        DT::datatable(rownames = F)
    })

    return(payment_df)
  })
}

## To be copied in the UI
# mod_model_calculation_ui("model_calculation_1")

## To be copied in the server
# mod_model_calculation_server("model_calculation_1")


# TODO clean up all this section



monthly_repayment = function (P, r, n) {
  round(P * r * ((1 + r ) ^ n) / ((1 + r) ^ n - 1), 2)
}

outstanding_balance = function (P, r, n, m) {
  round(P * ((1 + r) ^ n - (1 + r) ^ m) / ((1 + r) ^ n - 1), 0)
}


calculate_basic_payments <- function (mortgage_length = 25, loan_principle = 200000, interest_rate = 0.05) {
  months_vec = 0:(mortgage_length - 1) # number of months to calculate outstanding balance for

  tibble::tibble(month = months_vec,
                 monthly_payment = monthly_repayment(loan_principle, interest_rate, mortgage_length),
                 balance_remaining = outstanding_balance(loan_principle, interest_rate, mortgage_length, months_vec)) %>%
    dplyr::mutate(interest_split = balance_remaining * interest_rate,
                  equity_split = monthly_payment - interest_split)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mortgage_length PARAM_DESCRIPTION, Default: 25
#' @param loan_principle PARAM_DESCRIPTION, Default: 2e+05
#' @param deposit PARAM_DESCRIPTION, Default: 10000
#' @param interest_vec PARAM_DESCRIPTION, Default: c(4.5, 5, 4.5)
#' @param regular_payments PARAM_DESCRIPTION, Default: tibble::tibble(test = 100, test2 = 200)
#' @param monthly_capacity PARAM_DESCRIPTION, Default: 2000
#' @param overpayment_chosen PARAM_DESCRIPTION, Default: 0
#' @param rental_income PARAM_DESCRIPTION, Default: 0
#' @param initial_sum_remaining PARAM_DESCRIPTION, Default: 0
#' @param initial_emergency_fund PARAM_DESCRIPTION, Default: 0
#' @param emergency_target PARAM_DESCRIPTION, Default: 0
#' @param savings_target PARAM_DESCRIPTION, Default: 0
#' @param saving_OP_pct PARAM_DESCRIPTION, Default: 0
#' @param monthly_outgoings PARAM_DESCRIPTION, Default: 0
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[glue]{glue}}
#' @rdname calculate_complex_payments
#' @export
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom glue glue
calculate_complex_payments <- function (mortgage_length = 25,
                                        loan_principle = 200000,
                                        deposit = 10000,
                                        # interest_rate = 5 / 100 / 12,
                                        # initial_rate = 3 / 100 / 12,
                                        # initial_term = 3 * 12,
                                        interest_vec = c(4.5, 5, 4.5),
                                        regular_payments = tibble::tibble(test = 100, test2 = 200),
                                        monthly_capacity = 2000,
                                        overpayment_chosen = 0,
                                        rental_income = 0,
                                        initial_sum_remaining = 0,
                                        initial_emergency_fund = 0,
                                        emergency_target = 0,
                                        savings_target = 0,
                                        saving_OP_pct = 0,
                                        monthly_outgoings = 0) {


  print(glue::glue("----- Calculate_Complex_Payments() inputs -----"))
  print(glue::glue("mortgage_length: {mortgage_length}"))
  print(glue::glue("loan_principle: {loan_principle}"))
  print(glue::glue("deposit: {deposit}"))
  print(glue::glue("interest_vec: {paste0(interest_vec, collapse = ',')}"))
  print(glue::glue("regular_payments: {regular_payments}"))
  print(glue::glue("monthly_capacity: {monthly_capacity}"))
  print(glue::glue("overpayment_chosen: {overpayment_chosen}"))
  print(glue::glue("rental_income: {rental_income}"))
  print(glue::glue("initial_sum_remaining: {initial_sum_remaining}"))
  print(glue::glue("initial_emergency_fund: {initial_emergency_fund}"))
  print(glue::glue("emergency_target: {emergency_target}"))
  print(glue::glue("savings_target: {savings_target}"))
  print(glue::glue("saving_OP_pct: {saving_OP_pct}"))
  print(glue::glue("monthly_outgoings: {monthly_outgoings}"))
  print(glue::glue("----- End of inputs -----"))

  # clean input variables
  saving_OP_proportion = saving_OP_pct / 100
  regular_payments <- regular_payments / 12
  monthly_capacity <- monthly_capacity - sum(regular_payments)



  initial_overpayment <- initial_sum_remaining * (1 - saving_OP_proportion)
  initial_savings <- initial_sum_remaining * saving_OP_proportion

  months_vec = 0:(mortgage_length * 12 - 1) # number of months to calculate outstanding balance for

  calculated_vectors <- calculate_interest_and_repayments(mortgage_length, interest_vec, loan_principle)
  interest_vec <- calculated_vectors$interest
  repayment_vec <- calculated_vectors$repayment


  payment_df = tibble::tibble(month = months_vec,
                              monthly_payment = repayment_vec,
                              balance_remaining = loan_principle,
                              monthly_capacity = monthly_capacity,
                              overpayment = 0,
                              emergency_fund = initial_emergency_fund,
                              savings = initial_savings,
                              savings_added = 0,
                              emergency_fund_added = 0,
                              interest_split = 0,
                              equity_split = 0,
                              rental_income = rental_income,
                              general_spend = monthly_outgoings) %>%
    bind_cols(regular_payments)

  payment_df$overpayment[1] <- initial_overpayment
  payment_df$savings_added[1] <- initial_savings
  payment_df$emergency_fund_added[1] <- initial_emergency_fund


  payment_df <- payment_df %>%
    calculate_mortgage(interest_vec, emergency_target, savings_target, saving_OP_proportion, regular_payments)


  payment_df <- payment_df %>%
    mutate(equity_pct = equity_split / monthly_payment) %>%
    mutate(equity_paid_off = equity_split + overpayment) %>%
    mutate(total_equity_paid_off = cumsum(equity_paid_off) + deposit) %>%
    rowwise() %>%
    mutate(total_monthly_payment=sum(c_across(c("monthly_payment", "overpayment",
                                                              names(payment_df)[names(payment_df) %in% names(regular_payments)])),
                                            na.rm=T)) %>%
    ungroup()

  if (any(payment_df < 0, na.rm = T)) {
    payment_df <- payment_df %>%
      slice(0)
  }

  payment_df
}

calculate_interest_and_repayments <- function (m_length, interest_vec, loan_principle) {

  # get cleaned interest vector
  if (length(interest_vec) >= (m_length)) {
    interest_vec <- interest_vec[1:(m_length)]
  } else {
    interest_vec <- c(
      interest_vec,
      rep(interest_vec[length(interest_vec)], (m_length) - length(interest_vec))
    )
  }

  # convert from annual interest to monthly
  interest_vec <- rep(interest_vec, each = 12) / 100 / 12

  repayment_vec = interest_vec %>%
    purrr::map_dbl(~monthly_repayment(loan_principle, ., m_length * 12))

  return(list(
    interest = interest_vec,
    repayment = repayment_vec
  ))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param interest_vec PARAM_DESCRIPTION
#' @param emergency_target PARAM_DESCRIPTION
#' @param savings_target PARAM_DESCRIPTION
#' @param saving_OP_proportion PARAM_DESCRIPTION
#' @param regular_payments PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname calculate_mortgage
#' @export
#' @import dplyr
calculate_mortgage <- function(df, interest_vec, emergency_target, savings_target, saving_OP_proportion, regular_payments) {

  for (i in 1:nrow(df)) {

    # if after the first month then alter remaining balance from previous month's spend
    if (i > 1) {

      # take the previous month's equity split and overpayment from the balance
      df$balance_remaining[i] = df$balance_remaining[i - 1] -
        df$equity_split[i - 1] -
        df$overpayment[i - 1]

      df$overpayment[i] = df$monthly_capacity[i] -
        df$monthly_payment[i]

      # if there's less than one month's repayment left on the balance
      if (df$balance_remaining[i] < df$monthly_payment[i]) {
        # pay the final payment
        df$monthly_payment[i] <- df$balance_remaining[i]
        df$overpayment[i] <- 0
      }

      # if there's less than £50 remaining on the balance (ie mortgage is paid)
      if (df$balance_remaining[i] < 50) {
        df$monthly_payment[i] <- 0
        df$balance_remaining[i] <- 0
      }

      # apply spare money to the emergency fund if it's not at the target
      if (df$emergency_fund[i - 1] < emergency_target) {
        move_from_OP = min(df$overpayment[i],
                           emergency_target - df$emergency_fund[i - 1])

        df$overpayment[i] <- df$overpayment[i] - move_from_OP
        df$emergency_fund[i] <- df$emergency_fund[i - 1] + move_from_OP
        df$emergency_fund_added[i] <- move_from_OP

      } else {
        df$emergency_fund[i] <- df$emergency_fund[i - 1]
      }

      # apply spare money to the savings if it's not at the target
      if (df$savings[i - 1] < savings_target) {
        move_from_OP = min(df$overpayment[i] * saving_OP_proportion,
                           savings_target - df$savings[i - 1])

        df$overpayment[i] <- df$overpayment[i] - move_from_OP
        df$savings[i] <- df$savings[i - 1] + move_from_OP
        df$savings_added[i] <- move_from_OP

      } else {
        df$savings[i] <- df$savings[i - 1]
      }

    }

    # calculate interest and equity on the monthly payment
    df$interest_split[i] <- df$balance_remaining[i] * interest_vec[i]
    df$equity_split[i] <- df$monthly_payment[i] - df$interest_split[i]

    # if there's less than £50 remaining on the balance (ie mortgage is paid)
    if (df$balance_remaining[i] < 50) {
      df <- df %>%
        mutate(equity_pct = equity_split / monthly_payment) %>%
        mutate(equity_paid_off = equity_split + overpayment)  %>%
        rowwise() %>%
        mutate(
          total_monthly_payment = sum(
            c_across(
              c("monthly_payment", "overpayment",
                "savings_added", "emergency_fund_added",
                names(df)[names(df) %in% names(regular_payments)])
            ),
            na.rm=T
          )
        ) %>%
        ungroup() %>%
        slice(1:i)

      return(df)
    }
  }

  return(df)
}


apply_additional_factors <- function (payment_df,
                                      initial_costs = 0,
                                      initial_lump_sum = 0,
                                      emergency_target = 0,
                                      savings_target = 0,
                                      saving_OP_proportion = 50,
                                      monthly_outgoings = 0) {

  payment_df

}

output_model_summary <- function () {
  print(glue::glue("Model parameters: \n
           Property price: £{property_value}
           Deposit: £{deposit}
           Mortgage length: {mortgage_length / 12} years
           Interest rate: {round(100 * interest_rate * 12, 2)}%\n
           Monthly overpayment: £{overpayment_chosen}
           Years saved by overpaying: {round((mortgage_length - nrow(payment_df)) / 12, 2)} years
           Interest saved by overpaying: £{ifelse(overpayment_chosen == 0, 0, sum(payment_df_no_overpayment$interest_split) %>% round - sum(payment_df$interest_split) %>% round)}\n\n"))

  print(glue::glue("Monthly repayments: £{monthly_repayment(loan_principle, interest_rate, mortgage_length)}"))
  print(glue::glue("Total interest paid: £{sum(payment_df$interest_split) %>% round}"))
  print(glue::glue("Total equity from payments: £{sum(payment_df$equity_split) %>% round}
            Total equity from overpayments: £{sum(payment_df$overpayment) %>% round}"))


  print(payment_df)
}

