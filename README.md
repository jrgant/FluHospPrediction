# Predicting Flu Hospitalizations Paper

## Installation

1.  Download R-3.6.3 from [CRAN](https://cran.r-project.org/index.html).
2.  Download a .zip file of this repository from the GitHub *Clone* tab
    and unzip the archive into a location of your choice.
    1.  Another option would be to clone this repository to your
        computer. However, cloning takes a long time due to large file
        sizes and large version control history.
3.  Navigate to the root of the unzipped repository on your computer and
    start an R session using R-3.6.3. You should see messages related to
    the `renv` package.
4.  Once `renv` has installed itself, run to install all the packages
    required.
5.  If you receive an error message stating that R could not install
    `FluHospPrediction`, run

<!-- -->

    library(remotes)
    remotes::install_github("tlverse/sl3")
    remotes::install_github("jrgant/FluHospPrediction")

Please post an issue if you have trouble installing.

# Abstract

Accurate forecasts can inform response to outbreaks. Most efforts in
influenza forecasting have focused on predicting influenza-like
activity, but fewer on influenza-related hospitalizations. We conducted
a simulation study to evaluate a super learner’s predictions of three
seasonal measures of influenza hospitalizations in the United States:
peak hospitalization rate, peak hospitalization week, and cumulative
hospitalization rate. We trained an ensemble machine learning algorithm
on 15,000 simulated hospitalization curves and generated weekly
predictions. We compared the performance of the ensemble (weighted
combination of predictions from multiple prediction algorithms), the
best-performing individual prediction algorithm, and a naive prediction
(median of a simulated outcome distribution). Ensemble predictions
performed similarly to the naive predictions early in the season but
consistently improved as the season progressed for all prediction
targets. The best-performing prediction algorithm in each week typically
had similar predictive accuracy compared to the ensemble, but the
specific prediction algorithm selected varied by week. An ensemble super
learner improved predictions of influenza-related hospitalizations,
relative to a naive prediction. Future work should examine the super
learner’s performance using additional empirical data on
influenza-related predictors (e.g., influenza-like illness). The
algorithm should also be tailored to produce prospective probabilistic
forecasts of selected prediction targets.

## Package directory (one level deep):

    ## .
    ## ├── DESCRIPTION
    ## ├── FluHospPrediction.Rproj
    ## ├── LICENSE.md
    ## ├── Makefile
    ## ├── NAMESPACE
    ## ├── R
    ## │   ├── calendar_mgmt.R
    ## │   ├── loss_absolute_error.R
    ## │   ├── simcrv_funs.R
    ## │   ├── sl_procedure.R
    ## │   └── summaries.R
    ## ├── README.Rmd
    ## ├── README.md
    ## ├── data
    ## │   ├── cleaned
    ## │   └── raw
    ## ├── ensrisk-cv-insample_elastnetrf.png
    ## ├── ensrisk-cv-insample_main.png
    ## ├── ensrisk-cv-insample_sqerrloss.png
    ## ├── inst
    ## │   ├── 01_data_cleaning_empdat.R
    ## │   ├── 02_simulate_hospcurves.R
    ## │   ├── 03_create_analysis_dataset.R
    ## │   ├── 04_run_superlearner.R
    ## │   ├── 05_run_sqerrloss_sensitivity.R
    ## │   ├── 06.1_tables-figures-setup.R
    ## │   ├── 06.2_tables-figures-simul.R
    ## │   ├── 06.3_tables-figures-main.R
    ## │   ├── 06.4_tables-figures-sens-1se.R
    ## │   ├── 06.5_tables-figures-sens-elastrf.R
    ## │   ├── 06.6_tables-figures-sens-sqerrloss.R
    ## │   ├── 06.7_tables-figures-sens-comb.R
    ## │   ├── 07_run_sl_prospective.R
    ## │   ├── 08_run_sl_observed.R
    ## │   ├── 09_tables-figures-sub-prosp-obs.R
    ## │   ├── 10_ensemble-cv.R
    ## │   ├── 11_ensemble-cv-sqerrloss.R
    ## │   ├── 12_ensemble-cv-1se.R
    ## │   ├── 13_ensemble_optimism.R
    ## │   ├── 14_run_sl_observed_allseasons.R
    ## │   ├── 15_extract-obstrain-preds.R
    ## │   ├── 16_tables-figures-obstrain.R
    ## │   ├── 99_test-script.R
    ## │   └── fmt_emp_season.R
    ## ├── man
    ## │   ├── calendar_mgmt.Rd
    ## │   ├── loss_absolute_error.Rd
    ## │   ├── predcurves.Rd
    ## │   ├── simcrv.Rd
    ## │   ├── simdist.Rd
    ## │   ├── summary_functions.Rd
    ## │   └── super_learner_proc.Rd
    ## ├── renv
    ## │   ├── activate.R
    ## │   ├── library
    ## │   ├── settings.dcf
    ## │   └── staging
    ## └── renv.lock

## Code files

-   All analytic code files are stored in `/inst` directory.
-   Functions to handle calendar date management, curve simulation, and
    risk table formatting located in `/R`

## Data Sources

### FluSurv-NET

Chaves SS, Lynfield R, Lindegren ML, Bresee J, Finelli L. **The US
Influenza Hospitalization Surveillance Network**. *Emerg Infect Dis.*
2015 Sep;21(9):1543–50. Available from:
<http://dx.doi.org/10.3201/eid2109.141912>

**FluView: Influenza Hospitalization Surveillance Network**, Centers for
Disease Control and Prevention. WEBSITE. (Emerging Infections Program
data)
