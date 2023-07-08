#' output_MonthlyHeadroom UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
#' @importFrom shinycssloaders withSpinner
mod_output_MonthlyHeadroom_ui <- function(id, title = "Monthly Headroom", label = "tab_headroom"){
  ns <- NS(id)
  tabPanel(
    title = title,
    value = ns(label),
    plotlyOutput(ns("headroomPlot")) %>% withSpinner(color="#0dc5c1")
  )
}

#' output_MonthlyHeadroom Server Functions
#'
#' @noRd
#' @importFrom plotly ggplotly renderPlotly
mod_output_MonthlyHeadroom_server <- function(id, payment_df, initial_term_length,
                                              monthly_takehome, rental_income, total_outgoings){

  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$headroomPlot <- renderPlotly({
      initial_length = if (is.na(initial_term_length()) | initial_term_length() == 0) {
        NA
      } else {
        initial_term_length() + 1
      }
      ggplotly(
        plot_headroom(payment_df(),
                      monthly_takehome() + rental_income(),
                      total_outgoings()),
        tooltip = c("Category", "Spend")
      )
    })
  })
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param max_capacity PARAM_DESCRIPTION, Default: 0
#' @param disposable_spend PARAM_DESCRIPTION, Default: 0
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname plot_headroom
#' @export
#' @import dplyr
#' @import ggplot2
plot_headroom <- function (df, max_capacity = 0, disposable_spend = 0) {
  year_ends <- 0:nrow(df) %>%
    {.[. %% 12 == 0]}
  df %>%
    slice(-1) %>%
    mutate(year = month / 12) %>%
    mutate(disposable_spend = disposable_spend) %>%
    tidyr::pivot_longer(any_of(c("disposable_spend", "equity_split", "interest_split", "overpayment",
                                 "savings_added", "emergency_fund_added",
                                 "ground_rent", "service_charge", "insurance", "maintenance")),
                        names_to = "Category", values_to = "Spend") %>%
    mutate(Category = factor(Category,
                             levels = c("emergency_fund_added", "savings_added", "overpayment",
                                        "disposable_spend",
                                        "interest_split", "equity_split",
                                        "ground_rent", "service_charge", "insurance", "maintenance")),
           Category_groups = factor(Category,
                                    levels = c("emergency_fund_added", "savings_added", "overpayment",
                                               "disposable_spend",
                                               "interest_split", "equity_split",
                                               "ground_rent", "service_charge", "insurance", "maintenance"),
                                    labels = c("additional_payments", "additional_payments", "additional_payments",
                                               "disposable_spend",
                                               "committed_spend", "committed_spend",
                                               "committed_spend", "committed_spend", "committed_spend", "committed_spend"))) %>%
    ggplot(aes(x = month, y = Spend)) +
    geom_col(aes(fill = Category), width = 1) +
    geom_hline(yintercept = max_capacity, linetype = "dashed", alpha = 0.5) +
    geom_vline(xintercept = year_ends) +
    theme(axis.text.x = element_blank()) +
    labs(x = "Year", y = "Monthly Spend") +
    scale_y_continuous (labels= label_dollar(prefix="\u00A3"))  +
    scale_x_continuous(labels=function(x) x/12, breaks = seq(0, nrow(df), by = 12)) +
    theme_minimal() +
    ggtitle("Monthly payment splits, hover for more info. \n")


}
