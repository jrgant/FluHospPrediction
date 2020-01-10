# Predicting Flu Hospitalizations Paper

## Package directory (two levels deep):

```
+-- data
|   +-- cleaned
|   |   +-- empdat.csv
|   |   +-- hypothetical-curves.Rds
|   |   \-- uscrn.csv
|   \-- raw
|       +-- flu
|       \-- uscrn
+-- data_cleaning_empdat.R
+-- data_cleaning_uscrn.R
+-- data_cleaning_uscrn_dropscript.R
+-- data_create_analytic.R
+-- DESCRIPTION
+-- FluHospPrediction.Rproj
+-- LICENSE.md
+-- man
|   +-- simcrv.Rd
|   \-- simdist.Rd
+-- NAMESPACE
+-- R
|   \-- simcrv_funs.R
+-- README.Rmd
\-- trendfilter_curvesim.R

```


## Code files

| File | Description |
| :------------------------------- | :---------------------------------------------------------------------------------------------- |
| data_cleaning_empdat.R           | Clean empirical influenza data (ILINet, FluSurv-NET, WHO/NREVSS viral activity), and incorporate data on holidays (Thanksgiving, Christmas). |
| data_cleaning_uscrn_dropscript.R | Drop unneeded files from full FTP download of United States Climate Research Network (USCRN) station data. |
| data_cleaning_uscrn.R            | Clean USCRN data and format for merge into simulated hospitalization curve data. |
| data_create_analytic.R           |... in progress ... create analytic dataset |
| trendfilter_curvesim.R           | Fit trend filters to observed hospitalization curves and simulate hypothetical hospitalization curves based on these fits and the curve generation procedure. See `?simcrv`. |


## Data Sources


### FluSurv-NET

Chaves SS, Lynfield R, Lindegren ML, Bresee J, Finelli L. **The US Influenza Hospitalization Surveillance Network**. _Emerg Infect Dis._ 2015 Sep;21(9):1543–50. Available from: http://dx.doi.org/10.3201/eid2109.141912

**FluView: Influenza Hospitalization Surveillance Network**, Centers for Disease Control and Prevention. WEBSITE.


### WHO/NREVSS Viral Activity

National Center for Immunization and Respiratory Diseases (NCIRD). **U.S. Influenza Surveillance System: Purpose and Methods** [Internet]. Centers for Disease Control and Prevention. 2019 [cited 2020 Jan 9]. Available from: https://www.cdc.gov/flu/weekly/overview.htm


### ILINet

National Center for Immunization and Respiratory Diseases (NCIRD). **U.S. Influenza Surveillance System: Purpose and Methods** [Internet]. Centers for Disease Control and Prevention. 2019 [cited 2020 Jan 9]. Available from: https://www.cdc.gov/flu/weekly/overview.htm


### USCRN Temperature and Relative Humidity

Diamond HJ, Karl TR, Palecki MA, Baker CB, Bell JE, Leeper RD, et al. **U.S. Climate Reference Network after One Decade of Operations: Status and Assessment**. _Bull Am Meteorol Soc_ 2013;94:485–98. doi:10.1175/BAMS-D-12-00170.1.


### Historical Thanksgiving Dates

Wikipedia contributors. **Thanksgiving (United States)**. In: _Wikipedia, The Free Encyclopedia_ [Internet]. 30 Dec 2019 [cited 10 Jan 2020]. Available: https://en.wikipedia.org/w/index.php?title=Thanksgiving_(United_States)&oldid=933219107