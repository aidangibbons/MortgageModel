#' output_SummaryText UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList htmlOutput
#' @importFrom shinycssloaders withSpinner
mod_output_SummaryText_ui <- function(id, title = "Summary", label = "tab_summary"){
  ns <- NS(id)

  tabPanel(
    title = title,
    value = ns(label),
    fluidRow(
      htmlOutput(ns("htmlMainHeadline")) %>% withSpinner(color="#0dc5c1")
    ),
    fluidRow(
      column(4,
             htmlOutput(ns("htmlOverallSummary")),
      ),
      column(4,
             htmlOutput(ns("htmlUpfrontCosts")),
      ),
      column(4,
             htmlOutput(ns("htmlInitialTerm"))
      )
    )
  )

}

#' output_SummaryText Server Functions
#'
#' @noRd
#' @importFrom glue glue
#' @import shiny
mod_output_SummaryText_server <- function(id, payment_df, inp_mortgage, inp_interest, inp_housing, inp_finances, inp_costs
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$htmlMainHeadline <- renderUI({

      tagList(
        h2("Summary:"),
        generate_mortgage_length_text(payment_df, inp_finances$monthly_rent_and_savings),
        hr2()
      )
    })


    output$htmlOverallSummary <- renderUI({
      isolate(payment_df())
      tagList(
        h3("Mortgage Summary"),
        glue("Property Value: £{format(inp_mortgage$property_value(), , scientific=F)}"), br(),
        glue("Deposit: £{inp_mortgage$deposit()}"), br(),
        glue("Loan Principle: £{inp_mortgage$loan_principle()}"), br(),
        glue("Stamp Duty: £{inp_housing$stamp_duty()}"), br(),
        glue("Loan-to-value ratio: {round(100 * inp_mortgage$loan_principle() / inp_mortgage$property_value(), 1)}%"), br()
      )
    })

    output$htmlUpfrontCosts <- renderUI({
      pd <- payment_df()
      emergency_fund_txt <- glue("Inital emergency fund: £{round(inp_costs$initial_costs()['initial_emergency_fund'])}")
      remaining_funds_txt <- glue("Funds remaining (i.e. initial savings): £{round(inp_costs$initial_costs()['initial_sum_remaining'])}")

      if (inp_costs$initial_costs()['initial_emergency_fund'] < 0) {
        emergency_fund_txt <- strong(glue("Insufficient initial funds, by £{-round(inp_costs$initial_costs()['initial_emergency_fund'])}"))
        remaining_funds_txt <- ""
      }

      tagList(
        h3("Initial finances"),
        glue("Available funds: £{round(inp_costs$initial_costs()['total_initial_lump_sum'])}"), br(),
        glue("Total costs: £{round(inp_costs$initial_costs()['total_initial_costs'])}"), br(),
        glue("- Deposit: £{round(inp_mortgage$deposit())}"), br(),
        glue("- Stamp duty: £{round(inp_housing$stamp_duty())}"), br(),
        glue("- Other costs: £{round(inp_costs$initial_costs()['total_initial_costs']) - round(inp_mortgage$deposit()) - round(inp_housing$stamp_duty())}"), br(),
        emergency_fund_txt, br(),
        remaining_funds_txt
      )
    })

    output$htmlInitialTerm <- renderUI({
      max_month = inp_interest$initial_term_length() - 1

      if (max_month < 0) {
        return(tagList(
          h3("No initial term selected")
        ))
      }

      initial_term_df <- payment_df() %>%
        dplyr::filter(month <= max_month)

      total_equity_earned = round(sum(initial_term_df$equity_paid_off))
      total_equity = total_equity_earned + inp_mortgage$deposit()
      overpayment_equity = round(sum(initial_term_df$overpayment))
      total_interest = round(sum(initial_term_df$interest_split))
      total_savings = round(sum(initial_term_df$savings_added))
      total_emergency_fund = round(sum(initial_term_df$emergency_fund_added))
      average_repayment = round(mean(initial_term_df$monthly_payment))
      mortgage_remaining = inp_mortgage$loan_principle() - total_equity

      tagList(
        h3(glue("Inital term summary (first {inp_interest$initial_term_length()} years)")),
        glue("Total equity gained: £{total_equity_earned} (£{total_equity} total)"), br(),
        glue("Morgage remaining: £{mortgage_remaining}"), br(),
        glue("Total interest paid: £{total_interest}"), br(),
        br(),
        glue("Total savings gained: £{total_savings}"), br(),
        glue("Total emergency fund gained: £{total_emergency_fund}"), br(),
        br(),
        glue("Average monthly repayment: £{average_repayment}")
      )
    })

  })
}

generate_mortgage_length_text <- function (payment_df, total_available) {
  avg_OP = round(mean(payment_df()$overpayment, na.rm = T))
  final_duration = nrow(payment_df())
  final_duration_years = final_duration %/% 12
  final_duration_months = final_duration %% 12

  final_duration_text = if (final_duration_months == 0) {
    glue("{final_duration_years} years")
  } else {
    glue("{final_duration_years} years and {final_duration_months} months")
  }

  mortgage_length_txt <- if (final_duration == 0) {
    glue("Mortgage unaffordable with current 'Total Available Monthly' (finances tab) of £{total_available()}")
    } else if (avg_OP > 0) {
    glue("Mortgage paid off in {final_duration_text}, with an average overpayment of £{avg_OP} per month.")
  } else {
    glue("Mortgage paid off in {final_duration_text}")
  }


  mortgage_length_txt
}

## To be copied in the UI
# mod_output_SummaryText_ui("output_SummaryText_1")

## To be copied in the server
# mod_output_SummaryText_server("output_SummaryText_1")
