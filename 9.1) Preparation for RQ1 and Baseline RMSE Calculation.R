
############################################################
# Script: 9.1) Preparation for RQ1 and Baseline RMSE Calculation
# This script estimates RMSE for all models on an unbiased 50:50 split.
# Results serve as baseline values for robustness analysis (RQ1).
############################################################

##### 1) Load packages, data, and set seed #####

# Load required packages
library(gamlss)
library(randomForest)
library(xgboost)
library(ggplot2)
library(dplyr)

# Load cleaned and imputed tenant dataset
tenant_completed <- readRDS("data/tenant_completed.rds")

# Set seed for reproducibility
set.seed(42)

#####

##### 2) Create 50:50 train-test split #####

# Get number of observations
n_total <- nrow(tenant_completed)

# Sample 50% of the data for training set
train_indices <- sample(1:n_total, size = floor(0.5 * n_total), replace = FALSE)

# Split into training and test sets
train_base <- tenant_completed[train_indices, ]
test_base  <- tenant_completed[-train_indices, ]

#####

##### 3) Define model formulas #####

# Full linear model (LM_full): all predictors
formula_LM_full <- living_space ~ 
  region + employment_type + partner_status + age + gender + marital_status + second_home +
  contract_type + worktime_type + minijob + job_search + nationality +
  income_main_source + pension + income_assets + income_rent + building_type +
  rented_apartment + rent_total + building_size + household_size + rent_per_sqm +
  rent_burden + num_employed_hh + num_foreigners_hh + num_children_hh +
  hh_net_income + hh_head_gender + hh_head_nationality + hh_head_marital_status +
  hh_head_employment + hh_head_income_source + hh_head_income + hh_head_education +
  hh_head_residence + family_type

# Reduced linear model (LM_red): only significant predictors from LM_full
formula_LM_red <- living_space ~ 
  region + age + contract_type + income_rent + building_type + rent_total +
  building_size + household_size + rent_per_sqm + rent_burden +
  hh_net_income + hh_head_education + family_type

# GAM model: same as LM_red, but pb() spline for rent_per_sqm
formula_GAM <- living_space ~ 
  region + age + contract_type + income_rent + building_type + rent_total +
  building_size + household_size + pb(rent_per_sqm) + rent_burden +
  hh_net_income + hh_head_education + family_type

# Sigma model: selected predictors for dispersion modeling
sigma_formula <- ~ rent_total + rent_per_sqm + rent_burden + hh_net_income

#####

##### 4) Define RMSE function #####
# Computes the root mean squared error between true and predicted values
rmse <- function(true, pred) {
  sqrt(mean((true - pred)^2))
}

#####

##### 5) Train models on train_base (50:50 baseline split) #####

# LM_full
model_LM_full <- gamlss(formula_LM_full, data = train_base, family = NO)

# LM_red
model_LM_red <- gamlss(formula_LM_red, data = train_base, family = NO)

# GAM
model_GAM <- gamlss(formula_GAM, data = train_base, family = NO)

# GAMLSS
model_GAMLSS <- gamlss(formula_GAM, sigma.fo = sigma_formula, data = train_base, family = NO)

# GAMLSS_lin
model_GAMLSS_lin <- gamlss(formula_LM_red, sigma.fo = sigma_formula, data = train_base, family = NO)

# RF
RF_model <- randomForest(formula = formula_LM_full, data = train_base, ntree = 500)

# XGB
x_train <- model.matrix(formula_LM_red, data = train_base)[, -1]
x_test  <- model.matrix(formula_LM_red, data = test_base)[, -1]
y_train <- train_base$living_space
y_test  <- test_base$living_space

dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest  <- xgb.DMatrix(data = x_test)

XGB_model <- xgboost(
  data = dtrain,
  objective = "reg:squarederror",
  nrounds = 500,
  max_depth = 6,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8,
  verbose = 0
)

#####

##### 6) Predict on test data and compute RMSEs #####

# Compute RMSE for each model on the test set (50:50 baseline split)
baseline_rmse <- data.frame(
  Modell = c("LM_full", "LM_red", "GAM", "GAMLSS", "GAMLSS_lin", "RF", "XGB"),
  RMSE_baseline = c(
    rmse(y_test, predict(model_LM_full, newdata = test_base, type = "response")),
    rmse(y_test, predict(model_LM_red, newdata = test_base, type = "response")),
    rmse(y_test, predict(model_GAM, newdata = test_base, type = "response")),
    rmse(y_test, predict(model_GAMLSS, newdata = test_base, type = "response")),
    rmse(y_test, predict(model_GAMLSS_lin, newdata = test_base, type = "response")),
    rmse(y_test, predict(RF_model, newdata = test_base)),
    rmse(y_test, predict(XGB_model, newdata = dtest))
  )
)

print(baseline_rmse)

#####  

##### 7) Save baseline RMSE to file #####
saveRDS(baseline_rmse, "baseline_rmse.rds")

#####


