##############################################################
# Script: 8.2) Predictive Analysis with Importance Weighting
#
# Description:
# For each draw and bias level (sigma), models are trained on the corresponding
# biased training data. Parametric models (LM, GAM, GAMLSS) are estimated using
# importance weights, while non-parametric models (RF, XGB) are fitted on
# weighted resamples of the training data. The models are then used to predict
# the test data, and RMSE is computed to evaluate predictive accuracy across
# models and bias levels under explicit covariate shift correction.
##############################################################


##### 1) Load packages, data, and set seed #####

library(gamlss)
library(randomForest) 
library(xgboost)
library(ggplot2)
library(dplyr)

# Load biased data list (with weights and KL values)
bias_sigma_list_result_with_IW <- readRDS("biased_data/bias_sigma_list_IW.rds")

# Set seed for reproducibility
set.seed(42)

#####

##### 2) Define model formulas #####

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

##### 3) Define RMSE function #####
# Computes the root mean squared error between true and predicted values
rmse <- function(true, pred) {
  sqrt(mean((true - pred)^2))
}

#####

##### 4) Loop over all sigma levels and draws #####

for (sigma in names(bias_sigma_list_result_with_IW)) {
  for (draw in names(bias_sigma_list_result_with_IW[[sigma]])) {
    
    cat(">> Sigma =", sigma, "| Draw =", draw, "\n")
    
    ### Extract train and test data ###
    draw_element <- bias_sigma_list_result_with_IW[[sigma]][[draw]]
    
    train_name <- names(draw_element)[grepl("^train", names(draw_element))]
    training_5 <- draw_element[[train_name]]
    
    test_name <- names(draw_element)[grepl("^test", names(draw_element))]
    test <- draw_element[[test_name]]
    
    ### Prepare importance weights for parametric models ###
    iw_vector <- training_5$importance_weight
    iw_vector[is.na(iw_vector)] <- 1e-6              # Replace NA with small value
    iw_vector[iw_vector < 1e-6] <- 1e-6              # Truncate very small values
    
    ##### Estimate models with importance weights #####
    
    ## Linear model (full)
    model_LM_full <- gamlss(formula_LM_full, data = training_5, family = NO, weights = iw_vector)
    
    ## Linear model (reduced)
    model_LM_red <- gamlss(formula_LM_red, data = training_5, family = NO, weights = iw_vector)
    
    ## GAM (nonlinear rent_per_sqm)
    model_GAM <- gamlss(formula_GAM, data = training_5, family = NO, weights = iw_vector)
    
    ## GAMLSS (mu and sigma nonlinear)
    model_GAMLSS <- gamlss(formula_GAM, sigma.fo = sigma_formula, data = training_5, family = NO, weights = iw_vector)
    
    ## GAMLSS (mu linear, sigma nonlinear)
    model_GAMLSS_lin <- gamlss(formula_LM_red, sigma.fo = sigma_formula, data = training_5, family = NO, weights = iw_vector)
    
    ##### Resample training data for RF and XGB using importance weights #####
    
    # Normalize weights and resample
    iw_norm <- iw_vector / sum(iw_vector)
    n_train <- nrow(training_5)
    resample_indices <- sample(1:n_train, size = n_train, replace = TRUE, prob = iw_norm)
    training_resampled <- training_5[resample_indices, ]
    
    ## Random Forest (on resampled data)
    RF_model <- randomForest(formula = formula_LM_full, data = training_resampled, ntree = 500)
    
    ## XGBoost (on resampled data)
    x_train <- model.matrix(formula_LM_red, data = training_resampled)[, -1]
    x_test <- model.matrix(formula_LM_red, data = test)[, -1]
    y_train <- training_resampled$living_space
    y_test <- test$living_space
    
    dtrain <- xgb.DMatrix(data = x_train, label = y_train)
    dtest <- xgb.DMatrix(data = x_test)
    
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
    
    ##### Predict test data #####
    
    pred_LM_full      <- predict(model_LM_full,      newdata = test, type = "response")
    pred_LM_red       <- predict(model_LM_red,       newdata = test, type = "response")
    pred_GAM          <- predict(model_GAM,          newdata = test, type = "response")
    pred_GAMLSS       <- predict(model_GAMLSS,       newdata = test, type = "response")
    pred_GAMLSS_lin   <- predict(model_GAMLSS_lin,   newdata = test, type = "response")
    pred_RF           <- predict(RF_model,           newdata = test)
    pred_XGB          <- predict(XGB_model,          newdata = dtest)
    
    ##### Save RMSE results only #####
    
    result_df <- data.frame(
      Modell = c("LM_full", "LM_red", "GAM", "GAMLSS", "GAMLSS_lin", "RF", "XGB"),
      RMSE = c(
        rmse(y_test, pred_LM_full),
        rmse(y_test, pred_LM_red),
        rmse(y_test, pred_GAM),
        rmse(y_test, pred_GAMLSS),
        rmse(y_test, pred_GAMLSS_lin),
        rmse(y_test, pred_RF),
        rmse(y_test, pred_XGB)
      )
    )
    
    bias_sigma_list_result_with_IW[[sigma]][[draw]]$model_results <- result_df
    cat("   -> DONE\n")
  }
}

#####

##### 5) Save results #####
saveRDS(bias_sigma_list_result_with_IW, "biased_data/bias_sigma_list_result_with_IW.rds")

#####