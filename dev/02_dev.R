# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()
## Add modules ----
## Create a module infrastructure in R/
# golem::add_module(name = "input_update_button", with_test = TRUE, open = T) # Name of the module
# golem::add_module(name = "input_panel", with_test = TRUE, open = T) # Name of the module
# golem::add_module(name = "input_MortgageDetails", with_test = TRUE, open = F) # Name of the module
# golem::add_module(name = "input_InterestRates", with_test = TRUE, open = F) # Name of the module
# golem::add_module(name = "input_HousingPosition", with_test = TRUE, open = F) # Name of the module
# golem::add_module(name = "input_Finances", with_test = TRUE, open = F) # Name of the module
# golem::add_module(name = "input_Costs", with_test = TRUE, open = F) # Name of the module
# golem::add_module(name = "output_panel", with_test = TRUE, open = T) # Name of the module
# golem::add_module(name = "output_TotalAssets", with_test = TRUE, open = F) # Name of the module
# golem::add_module(name = "output_MonthlyHeadroom", with_test = TRUE, open = F) # Name of the module
# golem::add_module(name = "output_PaymentSplit", with_test = TRUE, open = F) # Name of the module
# golem::add_module(name = "output_Tables", with_test = TRUE, open = F) # Name of the module
# golem::add_module(name = "output_SummaryText", with_test = TRUE, open = F) # Name of the module

# golem::add_module(name = "model_calculation", with_test = TRUE, open = T) # Name of the module

usethis::use_package("dplyr", min_version = T)
usethis::use_package("tidyselect", min_version = T)
usethis::use_package("stringr", min_version = T)
usethis::use_package("DT", min_version = T)
usethis::use_package("purrr", min_version = T)
usethis::use_package("glue", min_version = T)
usethis::use_package("shinipsum", min_version = T)
usethis::use_package("tidyr", min_version = T)
usethis::use_package("ggplot2", min_version = T)
usethis::use_package("plotly", min_version = T)


## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("helpers", with_test = TRUE)
golem::add_utils("helpers", with_test = TRUE)
golem::add_fct("ui", with_test = T)

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "ui_config")

## Add resources ----
# add image to inst/app/img
# addResourcePath( 'img', system.file('app/img', package = 'golex') )
# Adding elements to your UI with tags$img(src = "img/name.png")
# addResourcePath('img', system.file('app/img', package = 'golex') )

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("MortgageModel")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
# usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
# usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()


# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
