
# Do Structural Assumptions Help in Biased Data Settings?  Comparing the Predictive Power of Parametric and Machine Learning Models

## Objective
This repository contains all R scripts required to replicate the empirical analyses conducted for the master's thesis. The central aim is to compare the predictive performance of parametric and machine learning models under various bias settings, with a particular focus on whether structural assumptions improve robustness when training data are subject to selection bias or covariate shift.

The analysis is based on real-world data from the German Microcensus 2010. Systematic distributional distortions between training and test data are deliberately introduced to examine model behavior under increasing bias levels.

In addition to the main scripts addressing the research questions (RQ1–RQ3), several supplementary scripts are included. These provide illustrative examples for key theoretical concepts such as nonlinear model structures, importance weighting, and covariate shift detection.


## Script Overview

### Main Scripts (Core Analysis)

1. **Generating `tenant_df` from the German Microcensus**  
   Variable selection and preprocessing to construct the working dataset.

2. **Imputation of Missing Values in `tenant_df`**  
   Single imputation of missing values using the `mice` package.

3. **Variable Selection for Bias Induction**  
   Selection of key predictors for bias scoring.

4. **Bias Induction**  
   Creation of systematically biased training datasets across different bias levels (Sigma).

5. **Visualization of Biased Variables**  
   Visual inspection of how key variables change under increasing bias.

   - 5.1: Biased variable `rent_per_sqm`  
   - 5.2: Biased variable `hh_net_income`  
   - 5.3: Biased variable `rent_total`  
   - 5.4: Target variable `living_space` under bias

6. **Model Selection for Parametric Approaches (Sigma = 5)**  
   Identification of suitable model specifications for LM, GAM, and GAMLSS based on the training data at a medium bias level (Sigma = 5).


7. **KL Divergence Estimation and Importance Weights**  
  Quantification of covariate shift using KL divergence and computation of importance weights.
8. **Predictive Analysis**  
  Out-of-sample predictions on systematically biased test data.
  
   - 8.1: Without bias correction  
   - 8.2: With importance weighting

9. **Research Question Scripts**

   - 9.1: Preparation for RQ1 and baseline RMSE estimation  
   - 9.2: RQ1 – Robustness score calculation and visualization  
   - 9.3: RQ2 – Effect of importance weighting on prediction accuracy  
   - 9.4: RQ2 – Effective sample size (ESS) computation  
   - 9.5: RQ3 – Relationship between KL divergence and RMSE

### Supplementary Scripts

A) **Comparison of Global and Local Models in a Nonlinear Setting**  
B) **Simulation Under Covariate Shift (σ = 1)**  
C) **Challenges of Importance Weighting**  
D) **KS-Test for Detection of Covariate Shift**  
E) **RMSE Plot Across Bias Levels**

## Requirements

- R version ≥ 4.2  
- Required packages: `gamlss`, `randomForest`, `xgboost`, `mgcv`, `mice`, `ggplot2`, `dplyr`, `FNN`

## Data

The analysis is based on the German Microcensus 2010 (Campus File), which is
available through the Research Data Centre of the Federal Statistical Office.
Further information and access options can be found at:

https://campus-file-fdz.nrw.de/

The Microcensus data are not included in this repository due to data access
restrictions.

To run the analysis, obtain the Microcensus 2010 Campus File and adjust the
file path in Script 1 accordingly.

## How to run

The scripts are intended to be executed in numerical order.

1. Obtain the German Microcensus 2010 Campus File.
2. Adjust the Microcensus file path in Script 1.
3. Run Scripts 1–4 to prepare the data and generate the biased datasets.
4. Run Scripts 5.1–9.5 for the visualizations, model selection, predictive
   analyses, and research question analyses.
5. Scripts A–E are supplementary simulations and analyses and can be run
   independently.


