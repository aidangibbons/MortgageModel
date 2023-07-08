#' output_PaymentSplit UI Function
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
mod_output_PaymentSplit_ui <- function(id, title = "Monthly Mortgage Breakdown", label = "tab_paymentsplits"){
  ns <- NS(id)
  tabPanel(
    title = title,
    value = ns(label),
    plotlyOutput(ns("paymentSplitPlot")) %>% withSpinner(color="#0dc5c1")
  )
}

#' output_PaymentSplit Server Functions
#'
#' @noRd
#' @importFrom plotly ggplotly renderPlotly
mod_output_PaymentSplit_server <- function(id, payment_df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$paymentSplitPlot <- renderPlotly({
      ggplotly(
        plot_payment_split_by_month(payment_df()),
        tooltip = c("payment_type", "year", "payment_split")
      )
    })
  })
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname plot_payment_split_by_month
#' @export
#' @import dplyr
#' @import ggplot2
plot_payment_split_by_month <- function (df) {
  df %>%
    pivot_longer(c(interest_split, equity_split, overpayment),
                 names_to = "payment_type", values_to = "payment_split") %>%
    mutate(year = month / 12) %>% #
    filter(year != 0) %>%
    ggplot(aes(x = year, y = payment_split)) +
    geom_point(aes(col = payment_type), size = 2) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    theme_minimal() +
    scale_y_continuous (labels= label_dollar(prefix="\u00A3"))
}
