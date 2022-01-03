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
## Add one line by package you want to add as dependency
usethis::use_package( "DT" )
usethis::use_pipe(export = TRUE)
usethis::use_package("broom")
usethis::use_package("bs4Dash")
usethis::use_package("dplyr")
usethis::use_package("ggplot2")
usethis::use_package("glmnet")
usethis::use_package("janitor")
usethis::use_package("knitr")
usethis::use_package("lubridate")
usethis::use_package("purrr")
usethis::use_package("qdapRegex")
usethis::use_package("readr")
usethis::use_package("readxl")
usethis::use_package("renv")
usethis::use_package("rmarkdown")
usethis::use_package("shinyFeedback")
usethis::use_package("shinyWidgets")
usethis::use_package("shinyjs")
usethis::use_package("stringr")
usethis::use_package("tidyr")
usethis::use_package("htmltools")
usethis::use_package("rlang")
usethis::use_package("tibble")
usethis::use_package("shinybusy")
usethis::use_package("rlang")



## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "sidebar" ) # Name of the module
golem::add_module( name = "calibrare" ) # Name of the module
golem::add_module(name = "coeficienti_portofoliu" )


## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct( "helpers" ) 
golem::add_utils(name = "functions_portofoliu",module = "database_portofoliu")

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("RiskManagement")
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
usethis::use_github_action_check_release() 
usethis::use_github_action_check_standard() 
usethis::use_github_action_check_full() 
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis() 
usethis::use_travis_badge() 

# AppVeyor 
usethis::use_appveyor() 
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

