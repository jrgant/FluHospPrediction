
# Predicting Flu Hospitalizations Paper

## Installation

Before installing this package, users will need to install the `sl3`
package from Github. All other dependencies should be installed
automatically.

``` r
library(remotes)
remotes::install_github("tlverse/sl3")
remotes::install_github("jrgant/FluHospPrediction")
```

Please post an issue if you have trouble installing.

## Abstract

Accurate forecasts of influenza epidemics can inform public health
response to outbreaks. To date, most efforts in influenza forecasting
have focused on predicting influenza-like activity. Fewer have sought to
predict other features of seasonal influenza epidemics, such as
influenza-related hospitalizations. We conducted a simulation study to
explore the potential for a machine-learning algorithm (super learner)
to predict three seasonal measures of influenza hospitalizations at the
national level in the United States: the peak hospitalization rate, the
week this peak hospitalization rate occurs, and the cumulative
hospitalization rate. We trained a super learner on 15,000 simulated
influenza hospitalization curves to generate predictions at each week of
the season for these seasonal prediction targets. For each week, we
compared the performance of the ensemble super learner (a weighted
combination of predictions from a set of individual prediction
algorithms), the discrete super learner (the best-performing individual
prediction algorithm), and a naive prediction based on the median of a
given outcome (e.g., median peak hospitalization rate). For the peak
hospitalization rate prediction target, we found that the super learner
algorithm consistently identified individual prediction algorithms that
improved upon a prediction using the median of the outcome distribution.
We found that the ensemble predictions in general performed slightly
better than the median prediction late in the flu season for the peak
hospitalization rate outcome and comparably earlier in the season. While
the discrete super learner during this period of the season typically
exhibited substantially lower prediction error than the ensemble super
learner, the component model selected as the discrete super learner
varied by week. In contrast, the ensemble performed stably throughout
the flu season. For the peak week outcome, the ensemble super learner’s
predictions exhibited slightly lower prediction error than the naive
median prediction. For the cumulative hospitalization rate outcome, the
ensemble super learner performed comparably to the median prediction
across all weeks, while individual discrete super learners exhibited
lower prediction error than the naive prediction, particularly late in
the flu season. This study suggests that the super learner may be a
useful tool for predicting influenza hospitalizations. Future work
should examine the super learner’s performance using empirical data and
additional influenza-related indicators such as influenza-like-illness
and viral activity. Whether the super learner is most useful for
selecting an individual prediction algorithm versus producing ensemble
predictions may vary by context: determining which uses of the super
learner, if any, might improve in-season forecasting of influenza
hospitalizations should be the focus of future applied investigations.

## Package directory (one level deep):

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
    ## |   +-- 04_run_superlearner.R
    ## |   +-- 05_run_sqerrloss_sensitivity.R
    ## |   +-- 06_tables-figures.R
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
    ## +-- README.Rmd
    ## \-- Rplots.pdf

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
