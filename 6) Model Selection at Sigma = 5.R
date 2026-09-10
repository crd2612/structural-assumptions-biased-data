
##############################################################
# Script: 6) Model Selection at Sigma = 5
#
# Description:
# This script performs model selection based on biased training data
# (Sigma = 5, Draw 1) and compares alternative model specifications
# using AIC calculated on the training set. The selected models serve
# as the basis for the subsequent predictive analysis across sigma levels.
##############################################################

##### 1) Load packages, data, and set seed #####

### Load required packages
library(gamlss)
library(mgcv)
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)

### Load biased training/test data (Sigma = 5)
bias_sigma_list <- readRDS("biased_data/bias_sigma_list.rds")

### Set seed for reproducibility
set.seed(42)

#####

##### 2) Select training and test data (Sigma = 5, Draw 1) #####

### Extract draw element for Sigma = 5, Draw 1
draw_element <- bias_sigma_list[["5"]][["Draw_1"]]

### Identify training and test sets
train_name <- names(draw_element)[grepl("^train", names(draw_element))]
training_5 <- draw_element[[train_name]]

test_name <- names(draw_element)[grepl("^test", names(draw_element))]
test <- draw_element[[test_name]]

#####

##### 3) Full linear model (LM_full) #####

### Inspect structure of training data
str(training_5)

### Estimate full model with all available predictors
LM_full <- gamlss(
  living_space ~ 
    region + 
    employment_type + 
    partner_status + 
    age + 
    gender + 
    marital_status + 
    second_home +
    contract_type + 
    worktime_type + 
    minijob + 
    job_search + 
    nationality +
    income_main_source + 
    pension + 
    income_assets + 
    income_rent + 
    building_type +
    rented_apartment + 
    rent_total + 
    building_size + 
    household_size + 
    rent_per_sqm +
    rent_burden + 
    num_employed_hh + 
    num_foreigners_hh + 
    num_children_hh +
    hh_net_income + 
    hh_head_gender + 
    hh_head_nationality + 
    hh_head_marital_status +
    hh_head_employment + 
    hh_head_income_source + 
    hh_head_income + 
    hh_head_education +
    hh_head_residence + 
    family_type,
  family = NO,
  data = training_5
)

### Display model summary
summary(LM_full)

#####

##### 4) Reduced linear model (LM_red) #####

### Define reduced model formula based on highly significant predictors in model_full
formula_red <- living_space ~ 
  region + 
  age + 
  contract_type + 
  income_rent + 
  building_type + 
  rent_total + 
  building_size + 
  household_size + 
  rent_per_sqm + 
  rent_burden + 
  hh_net_income + 
  hh_head_education + 
  family_type

### Estimate reduced model
LM_red <- gamlss(
  formula = formula_red,
  family = NO,
  data = training_5
)

#####

##### 5) Nonlinearity check and GAM #####

### Identify numeric predictors from reduced model
reduced_vars <- c(
  "region", "age", "contract_type", "income_rent", "building_type",
  "rent_total", "building_size", "household_size", "rent_per_sqm",
  "rent_burden", "hh_net_income", "hh_head_education", "family_type"
)

numeric_reduced_vars <- reduced_vars[
  sapply(training_5[reduced_vars], is.numeric)
]

### Fit exploratory GAM model to assess potential nonlinearity
model_gam_numeric <- gam(
  living_space ~ 
    s(rent_total) + 
    s(hh_net_income) + 
    s(rent_burden) + 
    s(rent_per_sqm) + 
    s(age),
  data = training_5,
  method = "REML"
)

### Plot smooth effects
plot(model_gam_numeric, pages = 1, rug = TRUE, se = TRUE)

### Based on visual inspection, nonlinearity is assumed only for:
# - rent_per_sqm

### Updated model formula using splines (pb()) for nonlinear terms
formula_GAM <- living_space ~ 
  region + 
  age + 
  contract_type + 
  income_rent + 
  building_type + 
  rent_total + 
  building_size + 
  household_size + 
  pb(rent_per_sqm) + 
  rent_burden + 
  hh_net_income + 
  hh_head_education + 
  family_type

### Fit GAM with NO distribution
GAM <- gamlss(
  formula = formula_GAM,
  family = NO,
  data = training_5
)

### Model summary
summary(GAM)

#####

##### 6) Analysis of sigma component (NO) #####

### Fit model with nonlinear mu-component and exploratory sigma-component
model_gamlss_sigma_test <- gamlss(
  formula = formula_GAM,
  sigma.fo = ~ 
    age + rent_total + household_size + rent_per_sqm + rent_burden + hh_net_income,
  family = NO,
  data = training_5
)

### Display model summary
summary(model_gamlss_sigma_test)

### Based on significance, the following sigma predictors are retained:
# sigma.fo = ~ rent_total + rent_per_sqm + rent_burden + hh_net_income

#####

##### 7) Final GAMLSS models with sigma component (NO) #####

### Final GAMLSS model:
# Nonlinear mu-component (pb splines) + selected sigma-component
model_final_sigma <- gamlss(
  formula = formula_GAM,
  sigma.fo = ~ rent_total + rent_per_sqm + rent_burden + hh_net_income,
  family = NO,
  data = training_5
)

summary(model_final_sigma)

#####

