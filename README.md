
# RiskManagement

<!-- badges: start -->
<!-- badges: end -->

The goal of RiskManagement is to automate provisions calculation within a credit risk department. The basic workflow is comprised of uploading data (excel or csv), processing data, output the processed data to the user
and saving data. The user will receive proper messages when processing uploaded data is not possible due to other missing files. The saved data is available afterwards for downloading or deletion. User rights are to be developed - a user will only see data and actions that she is entitled to. Since the app is developed with golem framework it is completely modularized and thus future development will be pretty straightforward. Currently the code lacks proper testing which is done internally.

## Installation

You can install the development version of RiskManagement from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fizic37/Risk-Management")
```
## IMPORTANT on installation
Installing the app will not work since the data used is private (stored within R/reactivedata folder) and not available on github. 

## Deplyment to production
Deployment to production is handled with docker image (built with Dockerfile inside the repository) and docker volumes. The docker is ran with the following command: docker run \-d \-it \-p 0.0.0.0:9000:80 \--name RiskReport -v risk_report:/build_zone/R/reactivedata/ -v baza_provizioane_plati:/build_zone/R/reactivedata/plati/external_volume/ -v portof_database:/build_zone/R/reactivedata/solduri/external_volume/ -v portofoliu_ifrs:/build_zone/R/reactivedata/solduri/external_volume_ifrs/ -v cereri_plata:/build_zone/R/reactivedata/plati/external_volume_cereri_plata/  \risk_report:latest

The docker volumes created and used by the app are: risk_report, baza_provizioane_plati,portof_database,
portofoliu_ifrs (last 3 volumes are also consumed by another app - RiskReport), cereri_plata (this volume is created for another app and consumed by current app).



