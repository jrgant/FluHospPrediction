Predicting Flu Hospitalizations Paper
=====================================

Installation
------------

Before installing this package, users will need to install the `sl3`
package from Github. All other dependencies should be installed
automatically.

    library(remotes)
    remotes::install_github("tlverse/sl3")
    remotes::install_github("jrgant/FluHospPrediction")

Please post an issue if you have trouble installing.

Package directory (one level deep):
-----------------------------------

    ## .
    ## +-- data
    ## |   +-- cleaned
    ## |   \-- raw
    ## +-- DESCRIPTION
    ## +-- FluHospPrediction.Rproj
    ## +-- inst
    ## |   +-- 01_data_cleaning_empdat.R
    ## |   +-- 02_simulate_hospcurves.R
    ## |   +-- 03_create_analysis_dataset.R
    ## |   +-- 04_eda.R
    ## |   +-- 05_superlearner_peakrate.R
    ## |   +-- 06_superlearner_peakweek.R
    ## |   +-- 06_superlearner_peakweek_MINI.R
    ## |   +-- 07_superlearner_cumhosp.R
    ## |   +-- 08_tables.R
    ## |   +-- 09_figures.R
    ## |   \-- 99_test-script.R
    ## +-- LICENSE.md
    ## +-- man
    ## |   +-- calendar_mgmt.Rd
    ## |   +-- loss_absolute_error.Rd
    ## |   +-- predcurves.Rd
    ## |   +-- simcrv.Rd
    ## |   +-- simdist.Rd
    ## |   +-- summary_functions.Rd
    ## |   \-- super_learner_proc.Rd
    ## +-- NAMESPACE
    ## +-- R
    ## |   +-- calendar_mgmt.R
    ## |   +-- loss_absolute_error.R
    ## |   +-- simcrv_funs.R
    ## |   +-- sl_procedure.R
    ## |   \-- summaries.R
    ## +-- README.md
    ## \-- README.Rmd

Code files
----------

-   All analytic code files are stored in /inst directory.
-   Functions to handle calendar date management and curve simulation
    located in /R

Data Sources
------------

### FluSurv-NET

Chaves SS, Lynfield R, Lindegren ML, Bresee J, Finelli L. **The US
Influenza Hospitalization Surveillance Network**. *Emerg Infect Dis.*
2015 Sep;21(9):1543–50. Available from:
<http://dx.doi.org/10.3201/eid2109.141912>

**FluView: Influenza Hospitalization Surveillance Network**, Centers for
Disease Control and Prevention. WEBSITE. (Emerging Infections Program
data)
