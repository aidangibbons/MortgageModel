#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      useShinyjs(),
      titlePanel("Mortgage Model"),
      sidebarLayout(
        sidebarPanel2(
          mod_input_update_button_ui("input_button"),
          br(), br(),
          mod_input_panel_ui("main")
        ),
        mainPanel(
          tabsetPanel(
            mod_output_SummaryText_ui("output_summarytext", "Summary", "tab_summary"),
            mod_output_TotalAssets_ui("output_assets", "Total Assets", "tab_assets"),
            mod_output_MonthlyHeadroom_ui("output_headroom", "Monthly Headroom", "tab_headroom"),
            mod_output_PaymentSplit_ui("output_paymentsplit", "Monthly Mortgage Breakdown", "tab_paymentsplits"),
            mod_model_calculation_ui("calculations", "Table Breakdown", "tab_tables"),
            tabPanel("Disclaimer",
                     h1("Disclaimer"),
                     p("Please be aware this app is simply a tool for trying to experiment with different scenarios. I cannot guarantee that there are no bugs within the code, and nothing within this app constitutes financial advice."))
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "MortgageModel"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
