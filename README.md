# README — A harmonized dataset of education- and age-specific fertility rates for DHS countries

Afua Durowaa-Boateng; Dilek Yildiz; Anne Goujon  
Dataset DOI: https://doi.org/10.5281/zenodo.16979890


## Overview
This repository hosts harmonized education- and age-specific fertility rates (EAFR) and derived education-specific total fertility rates (ETFR) for 79 DHS countries, provided in five-year periods from 1970 to 2020 and four education levels (No, Primary, Secondary, Higher). Estimates are obtained via (i) GLM-based initial EAFR from DHS birth histories (Stata `tfr2`) and (ii) a three-level hierarchical Bayesian model that jointly estimates across regions and enforces UN WPP-consistency using education–age female population weights (WIC).


The Stata `tfr2` module operates on IR (Individual Recode) files from the Demographic and Health Surveys (DHS) program, which offers data for free after an initial registration (see https://dhsprogram.com/).

## Contents:
- estimates from the final model `BESFR_estimates global_south.xlsx`
- R code to compare the precisions from alternate models `Bayesian model comparisons for precision.R`
- the main/final model `Bayesian model global_south.R`
-validation codes for the model
`Bayesian model global_south_validation country omission.R`
`Bayesian model global_south_validation.R`

- sensitivity analysis/alternative models comparisons `Sensitivity and comparison plots.R`
- input data for the WIC population sizes by education `WIC_datasets.xlsx`
- educational differences output `edu_diff.xlsx`
- priors `edu_spec_sd_init_prior.xlsx`
  
- ## alternative models in R
`glm input data global south model_alpha_beta.R`
`glm input data global south model_edu_spec_mean.R`
`glm input data global south model_edu_spec_sd.R`
`glm input data global south model_gamma_47.R`
`glm input data global south model_pred_prec.R`
`glm input data global south model_se_sd_c_y_a.R`
`glm input data global south model_se_sd_c_y_e.R`

## Software
- Stata with `tfr2` for ASFR tabulations  
- R (version ≥ 4.x); key packages include `wpp2022`, `wcde`
