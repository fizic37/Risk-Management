
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
Deployment to production is handled with docker image (built with Dockerfile inside the repository) and docker volumes. The docker is ran with the following command: docker run \-d \-it \-p 0.0.0.0:9001:80 \--name RiskManagement -v RiskManagement:/build_zone/R/reactivedata -v baza_provizioane_plati:/build_zone/R/reactivedata/plati/ -v portof_database:/build_zone/R/reactivedata/portofoliu/ -v portofoliu_ifrs:/build_zone/R/reactivedata/ifrs/ -v cereri_plata:/build_zone/R/plati/external_volume/ risk_management:latest
where risk_management is the docker image built with docker build -t risk_management -f Dockerfile Risk-Management/Dockerfile

The docker volumes created and used by the app are: RiskManagement, baza_provizioane_plati,portof_database,portofoliu_ifrs. These volumes are fed with data by the app and the last 3 volumes are also used by RiskReport app ( also available within my repository).
Docker volume cereri_plata is used by the app but is fed with new data by the RiskReport app.



