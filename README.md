---
output: 'github\_document'
...

Predicting Flu Hospitalizations Paper
=====================================

Installation
------------

Before installing this package, users will need to install the `glmgen`
package, which is not on CRAN but is a dependency:

`{r, eval = F} library(remotes) remotes::install_github("statsmaths/glmgen", subdir = "R_pkg/glmgen") remotes::install_github("jrgant/FluHospPrediction")`

Package directory (two levels deep):
------------------------------------

```
.
+-- data
|   +-- cleaned
|   |   +-- analytic_datasets.Rds
|   |   +-- empdat.csv
|   |   +-- empdat_weeksum.csv
|   |   \-- hypothetical-curves.Rds
|   \-- raw
|       \-- flu
+-- DESCRIPTION
+-- FluHospPrediction.Rproj
+-- inst
|   +-- 01_data_cleaning_empdat.R
|   +-- 02_simulate_hospcurves.R
|   \-- 03_create_analysis_dataset.R
+-- LICENSE.md
+-- man
|   +-- calendar_mgmt.Rd
|   +-- predcurves.Rd
|   +-- simcrv.Rd
|   \-- simdist.Rd
+-- NAMESPACE
+-- R
|   +-- calendar_mgmt.R
|   \-- simcrv_funs.R
+-- README.md
\-- README.Rmd
```

Code files
----------

All code files are stored in the /inst directory.

  File                                  Description
  ------------------------------------- ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  data\_cleaning\_empdat.R              Clean empirical influenza data (ILINet, FluSurv-NET, WHO/NREVSS viral activity), and incorporate data on holidays (Thanksgiving, Christmas).
  data\_cleaning\_uscrn\_dropscript.R   Drop unneeded files from full FTP download of United States Climate Research Network (USCRN) station data.
  data\_cleaning\_uscrn.R               Clean USCRN data and format for merge into simulated hospitalization curve data.
  data\_create\_analytic.R              ... in progress ... create analytic dataset
  trendfilter\_curvesim.R               Fit trend filters to observed hospitalization curves and simulate hypothetical hospitalization curves based on these fits and the curve generation procedure. See `?simcrv`.

Data Sources
------------

### FluSurv-NET

Chaves SS, Lynfield R, Lindegren ML, Bresee J, Finelli L. **The US
Influenza Hospitalization Surveillance Network**. *Emerg Infect Dis.*
2015 Sep;21(9):1543–50. Available from:
http://dx.doi.org/10.3201/eid2109.141912

**FluView: Influenza Hospitalization Surveillance Network**, Centers for
Disease Control and Prevention. WEBSITE. (Emerging Infections Program
data)

### WHO/NREVSS Viral Activity

National Center for Immunization and Respiratory Diseases (NCIRD).
**U.S. Influenza Surveillance System: Purpose and Methods** [Internet].
Centers for Disease Control and Prevention. 2019 [cited 2020 Jan 9].
Available from: https://www.cdc.gov/flu/weekly/overview.htm

### ILINet

National Center for Immunization and Respiratory Diseases (NCIRD).
**U.S. Influenza Surveillance System: Purpose and Methods** [Internet].
Centers for Disease Control and Prevention. 2019 [cited 2020 Jan 9].
Available from: https://www.cdc.gov/flu/weekly/overview.htm

### USCRN Temperature and Relative Humidity

Diamond HJ, Karl TR, Palecki MA, Baker CB, Bell JE, Leeper RD, et al.
**U.S. Climate Reference Network after One Decade of Operations: Status
and Assessment**. *Bull Am Meteorol Soc* 2013;94:485–98.
doi:10.1175/BAMS-D-12-00170.1.

### Historical Thanksgiving Dates

Wikipedia contributors. **Thanksgiving (United States)**. In:
*Wikipedia, The Free Encyclopedia* [Internet]. 30 Dec 2019 [cited 10 Jan
2020]. Available:
https://en.wikipedia.org/w/index.php?title=Thanksgiving\_(United\_States)&oldid=933219107
