## code to prepare `ui_config` dataset goes here

ui_config <-
  tibble::tibble(
    # mod_input_MortgageDetails
    txtPropertyValue = "£300000",
    txtDeposit = "10%",
    rdoDepositFormat = "%",
    txtMortgageLength = "35",

    # mod_input_InterestRates
    rdoInterestType = "Fixed",
    txtInitialTerm = "2",
    txtInitialInterestRate = "5.9%",
    txtInterestRate = "7.9%",
    txtTrackerRates = "5.7, 6, 4.5, 3",

    # mod_input_HousingPosition
    chkFTB = T,
    chkSellingHouse = F,
    txtCurrentHouseValue = "0",

    # mod_input_Finances
    txtTotalLiquid = "74000",
    txtTotalRentSavings = "2500",
    txtTotalOutgoings = "2000",
    txtRent = "0",
    txtRentalTax = "30",
    txtEmergencyFund = "10000",
    txtSavingsTarget = "10000",
    txtSavingsOverpaymentSplit = "25",
    txtOverpayment = "£0",
    chkOverpaymentToggle = T,
    txtOverpaymentLimit = "10%",

    # mod_input_Costs
    txtSurveyFee = "£500",
    txtSolicitorFee = "£1200",
    txtMortgageFee = "£1000",
    txtMortgageAdvisorFee = "£395",
    txtMovingCosts = "£400",
    txtLandRegistry = "£300",
    txtAdditionalCosts = "£2500",
    txtGroundRent = "£0",
    txtServiceCharge = "£0",
    txtInsurance = "£500",
    txtMaintenance = "£3000"
  )

usethis::use_data(ui_config, overwrite = TRUE)
sinew::makeOxygen(ui_config)



