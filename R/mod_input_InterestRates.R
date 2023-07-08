#' input_InterestRates UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_InterestRates_ui <- function(id, title = "Interest Rates", label = "tab_interestrates"){
  ns <- NS(id)

  tabPanel(
    title = title,
    label = ns(label),
    br(),
    radioButtons(ns("rdoInterestType"), "Interest rate type: ", c("Fixed", "Tracker"),
                 selected = ui_config$rdoInterestType, inline = T),
    textInput(ns("txtInitialTerm"), label = "Inital term (years): ", value = ui_config$txtInitialTerm),
    h3("Fixed-rate options"),
    textInput(ns("txtInitialInterestRate"), label = "Initial term interest rate (%): ", value = ui_config$txtInitialInterestRate),
    textInput(ns("txtInterestRate"), label = "Annual interest rate (%): ", value = ui_config$txtInterestRate),
    h3("Tracker options"),
    textInput(ns("txtTrackerRates"),
              "Enter tracker rates. One value per year, separated by commas (e.g. 4,5.5,4). The final value will be extended over the mortgage length",
              value = ui_config$txtTrackerRates),
    htmlOutput(ns("txtChosenRate"))
    # interest rate forecast: https://think.ing.com/forecasts
  )
}

#' input_InterestRates Server Functions
#'
#' @noRd
#'
#' @importFrom shiny tagList h2
mod_input_InterestRates_server <- function(id, btnUpdate){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    interest_rate = reactive({input$txtInterestRate %>% clean_numeric_input}) # . / 100 / 12

    initial_term_interest = reactive({input$txtInitialInterestRate %>% clean_numeric_input}) #  %>% {. / 100 / 12}
    initial_term_length = reactive({input$txtInitialTerm %>% clean_numeric_input}) #  %>% {. * 12}

    tracker_rates_raw <- reactive({input$txtTrackerRates})

    interest_rates_raw <- reactive({
      paste(
        paste(
          rep(initial_term_interest(), initial_term_length()),
          collapse = ","
        ),
        interest_rate(),
        collapse = ",",
        sep = ","
      )
    })

    rate_vec_raw <- reactive({
      if (input$rdoInterestType == "Fixed") {
        interest_rates_raw()
      } else if (input$rdoInterestType == "Tracker") {
        tracker_rates_raw()
      }
    })

    rate_vec <- reactive({
      rate_vec_raw() %>%
        stringr::str_split(",") %>%
        {.[[1]]} %>%
        stringr::str_trim() %>%
        clean_numeric_input(0) # %>% {. / 100 / 12}
    })

    output$txtChosenRate <- renderUI({
      tagList(
        h2("Chosen rates: "),
        rate_vec_raw()
      )
    })

    initial_term_length_output <- eventReactive({btnUpdate()
      session$clientData}, {
      initial_term_length()
    })

    return(
      list(
        rate_vec = rate_vec,
        initial_term_length = initial_term_length_output
      )
    )
  })
}

## To be copied in the UI
# mod_input_InterestRates_ui("input_InterestRates_1")

## To be copied in the server
# mod_input_InterestRates_server("input_InterestRates_1")
