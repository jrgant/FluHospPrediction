
# Predicting Flu Hospitalizations Paper

## Installation

Before installing this package, users will need to install the `glmgen`
package, which is not on CRAN but is a dependency:

``` r
library(remotes)
remotes::install_github("statsmaths/glmgen", subdir = "R_pkg/glmgen")
remotes::install_github("jrgant/FluHospPrediction")
```

## Package directory (two levels deep):

    ## .
    ## +-- data
    ## |   +-- cleaned
    ## |   |   +-- analytic_datasets.Rds
    ## |   |   +-- empdat.csv
    ## |   |   +-- empdat_weeksum.csv
    ## |   |   \-- hypothetical-curves.Rds
    ## |   \-- raw
    ## |       \-- flu
    ## +-- DESCRIPTION
    ## +-- FluHospPrediction.Rproj
    ## +-- inst
    ## |   +-- 01_data_cleaning_empdat.R
    ## |   +-- 02_simulate_hospcurves.R
    ## |   \-- 03_create_analysis_dataset.R
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
    ## \-- README.Rmd

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
