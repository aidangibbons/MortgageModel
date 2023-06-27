## code to prepare `ui_config` dataset goes here

ui_config <-
  tibble::tibble(
    "txtPropertyValue" = "£300000",
    "txtDeposit" = "10%",
    "rdoDepositFormat" = "%",
    "txtMortgageLength" = "35"
  )

usethis::use_data(ui_config, overwrite = TRUE)
sinew::makeOxygen(ui_config)



