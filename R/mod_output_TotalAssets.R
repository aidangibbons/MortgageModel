
#' output_TotalAssets UI Function
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
mod_output_TotalAssets_ui <- function(id, title = "Total Assets", label = "tab_assets"){
  ns <- NS(id)

  tabPanel(
    title = title,
    value = ns(label),
    plotlyOutput(ns("assetPlot")) %>% withSpinner(color="#0dc5c1")
  )
}

#' output_TotalAssets Server Functions
#'
#' @noRd
#' @importFrom plotly renderPlotly ggplotly layout
mod_output_TotalAssets_server <- function(id, payment_df, deposit, mortgage_length, initial_term_length){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$assetPlot <- renderPlotly({
      ggplotly(
        plot_assets(payment_df(),
                    deposit = deposit(),
                    max_length = mortgage_length(),
                    initial_term = initial_term_length()
        ),
        tooltip = c("Asset")
      ) %>%
        layout(xaxis = list(spikemode = 'across'),
               yaxis = list(spikemode = 'across'))

    })

  })
}

## To be copied in the UI
# mod_output_TotalAssets_ui("output_TotalAssets_1")

## To be copied in the server
# mod_output_TotalAssets_server("output_TotalAssets_1")

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param deposit PARAM_DESCRIPTION
#' @param max_length PARAM_DESCRIPTION, Default: 25
#' @param initial_term PARAM_DESCRIPTION, Default: 3
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{select}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{mutate-joins}}
#'  \code{\link[tidyr]{pivot_longer}}
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[scales]{label_dollar}}
#' @rdname plot_assets
#' @export
#' @importFrom dplyr select mutate left_join
#' @importFrom tidyr pivot_longer
#' @importFrom tibble tibble
#' @importFrom scales label_dollar
#' @import ggplot2
plot_assets  <- function (df, deposit, max_length = 25, initial_term = 3) {


  df_formatted <- df %>%
    dplyr::select(month, "total_equity" = equity_paid_off, emergency_fund, savings) %>%
    dplyr::mutate(total_equity = c(total_equity[1] + deposit, total_equity[-1])) %>%
    dplyr::mutate(total_equity = cumsum(total_equity)) %>%
    tidyr::pivot_longer(c(total_equity, emergency_fund, savings), names_to = "Asset", values_to = "Value")

  months_vec = 0:(12 * max_length - 1)

  max_val = max(df_formatted %>%
                  dplyr::group_by(month) %>%
                  summarise(tot = sum(Value, na.rm = T)) %>%
                  pull(tot))

  df_to_plot <- tibble::tibble(month = months_vec) %>%
    dplyr::mutate(month_join = ifelse(month > max(df_formatted$month),
                                      max(df_formatted$month),
                                      month)) %>%
    dplyr::left_join(df_formatted, by = c("month_join" = "month")) %>%
    dplyr::mutate(year = month / 12)

  df_to_plot %>%
    ggplot(aes(x = year, y = Value)) +
    geom_vline(xintercept = initial_term, alpha = 0.4, linetype = "dashed", size = 2) +
    geom_text(label = "Initial Term", y = max_val, x = initial_term) +
    geom_vline(xintercept = df_to_plot$year[df_to_plot$year %% 1 == 0], alpha = 0.3) +
    geom_area(aes(fill = Asset, group = Asset)) +
    theme_minimal() +
    scale_y_continuous(labels = scales::label_dollar(prefix="\u00A3")) +
    scale_x_continuous(breaks = df_to_plot$year[df_to_plot$year %% 1 == 0])
}
