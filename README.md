
# Predicting Flu Hospitalizations Paper

## Installation

Before installing this package, users will need to install the `glmgen`
and `sl3` packages, which are not on CRAN but are dependencies:

``` r
library(remotes)
remotes::install_github("statsmaths/glmgen", subdir = "R_pkg/glmgen")
remotes::install_github("tlverse/sl3")
remotes::install_github("jrgant/FluHospPrediction")
```

Please post an issue if you have trouble installing.

## Package directory (two levels deep):

    ## .
    ## +-- data
    ## |   +-- cleaned
    ## |   |   +-- analytic_datasets.Rds
    ## |   |   +-- empdat.csv
    ## |   |   +-- empdat_weeksum.csv
    ## |   |   +-- hypothetical-curves.Rds
    ## |   |   +-- sim_dataset_analytic_week_01.Rds
    ## |   |   +-- sim_dataset_analytic_week_02.Rds
    ## |   |   +-- sim_dataset_analytic_week_03.Rds
    ## |   |   +-- sim_dataset_analytic_week_04.Rds
    ## |   |   +-- sim_dataset_analytic_week_05.Rds
    ## |   |   +-- sim_dataset_analytic_week_06.Rds
    ## |   |   +-- sim_dataset_analytic_week_07.Rds
    ## |   |   +-- sim_dataset_analytic_week_08.Rds
    ## |   |   +-- sim_dataset_analytic_week_09.Rds
    ## |   |   +-- sim_dataset_analytic_week_10.Rds
    ## |   |   +-- sim_dataset_analytic_week_11.Rds
    ## |   |   +-- sim_dataset_analytic_week_12.Rds
    ## |   |   +-- sim_dataset_analytic_week_13.Rds
    ## |   |   +-- sim_dataset_analytic_week_14.Rds
    ## |   |   +-- sim_dataset_analytic_week_15.Rds
    ## |   |   +-- sim_dataset_analytic_week_16.Rds
    ## |   |   +-- sim_dataset_analytic_week_17.Rds
    ## |   |   +-- sim_dataset_analytic_week_18.Rds
    ## |   |   +-- sim_dataset_analytic_week_19.Rds
    ## |   |   +-- sim_dataset_analytic_week_20.Rds
    ## |   |   +-- sim_dataset_analytic_week_21.Rds
    ## |   |   +-- sim_dataset_analytic_week_22.Rds
    ## |   |   +-- sim_dataset_analytic_week_23.Rds
    ## |   |   +-- sim_dataset_analytic_week_24.Rds
    ## |   |   +-- sim_dataset_analytic_week_25.Rds
    ## |   |   +-- sim_dataset_analytic_week_26.Rds
    ## |   |   +-- sim_dataset_analytic_week_27.Rds
    ## |   |   +-- sim_dataset_analytic_week_28.Rds
    ## |   |   +-- sim_dataset_analytic_week_29.Rds
    ## |   |   +-- sim_dataset_analytic_week_30.Rds
    ## |   |   +-- sim_dataset_long.csv
    ## |   |   \-- sim_dataset_wide.csv
    ## |   \-- raw
    ## |       \-- flu
    ## +-- DESCRIPTION
    ## +-- FluHospPrediction.Rproj
    ## +-- inst
    ## |   +-- 01_data_cleaning_empdat.R
    ## |   +-- 02_simulate_hospcurves.R
    ## |   +-- 03_create_analysis_dataset.R
    ## |   +-- 04_eda.R
    ## |   \-- 05_superlearner.R
    ## +-- LICENSE.md
    ## +-- man
    ## |   +-- calendar_mgmt.Rd
    ## |   +-- predcurves.Rd
    ## |   +-- simcrv.Rd
    ## |   \-- simdist.Rd
    ## +-- NAMESPACE
    ## +-- R
    ## |   +-- calendar_mgmt.R
    ## |   \-- simcrv_funs.R
    ## +-- README.md
    ## +-- README.Rmd
    ## \-- scratch.R

## Code files

  - All analytic code files are stored in /inst directory.
  - Functions to handle calendar date management and curve simulation
    located in /R

## Data Sources

### FluSurv-NET

Chaves SS, Lynfield R, Lindegren ML, Bresee J, Finelli L. **The US
Influenza Hospitalization Surveillance Network**. *Emerg Infect Dis.*
2015 Sep;21(9):1543–50. Available from:
<http://dx.doi.org/10.3201/eid2109.141912>

**FluView: Influenza Hospitalization Surveillance Network**, Centers for
Disease Control and Prevention. WEBSITE. (Emerging Infections Program
data)
