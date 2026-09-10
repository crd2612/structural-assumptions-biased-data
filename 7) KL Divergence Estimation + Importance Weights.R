##############################################################
# Script: 7) KL Divergence Estimation + Importance Weights
#
# Description:
# This script computes KL divergence values and corresponding
# importance weights between biased training and unbiased test data
# using the KLIEP method. Results are stored in a new list
# (bias_sigma_list_IW), which serves as input for later prediction
# models with or without bias correction.
##############################################################

##### 1) Load packages, data, and set seed #####

library(dplyr)
library(densratio)

# Load biased data list
bias_sigma_list <- readRDS("biased_data/bias_sigma_list.rds")

# Set seed for reproducibility
set.seed(42)

#####

##### 2) Define KLIEP estimation function #####

estimate_kliep_pair <- function(train_df, test_df, vars) {
  train_sample <- train_df[, vars]
  test_sample  <- test_df[, vars]
  
  # Joint standardization
  combined_sample <- rbind(train_sample, test_sample)
  combined_scaled <- scale(combined_sample)
  
  P_sample <- combined_scaled[1:nrow(train_sample), ]
  Q_sample <- combined_scaled[(nrow(train_sample)+1):nrow(combined_sample), ]
  
  # Add small jitter to avoid numerical issues
  jitter_sd <- 1e-6
  P_sample <- P_sample + matrix(rnorm(length(P_sample), 0, jitter_sd), ncol = ncol(P_sample))
  Q_sample <- Q_sample + matrix(rnorm(length(Q_sample), 0, jitter_sd), ncol = ncol(Q_sample))
  
  # Estimate KL divergence via KLIEP method (Sugiyama et al., 2008)
  w_model <- densratio(P_sample, Q_sample, method = "KLIEP")
  log_ratios <- log(w_model$compute_density_ratio(P_sample))
  kliep_val <- mean(log_ratios)
  importance_weights <- w_model$compute_density_ratio(P_sample)
  
  
  return(list(kliep_value = kliep_val, importance_weights = importance_weights))
}

#####

##### 3) Create new list for KLIEP-based importance weights #####

bias_sigma_list_IW <- bias_sigma_list  

#####

##### 4) Compute KLIEP-based importance weights and KL divergence estimates #####

# Variables used for KLIEP estimation (joint distribution shift)
bias_vars <- c("rent_total", "hh_net_income", "rent_per_sqm")

# Loop over all bias levels (sigma)
for (sigma in names(bias_sigma_list_IW)) {
  
  # Loop over all draws within each sigma level
  for (draw_name in names(bias_sigma_list_IW[[sigma]])) {
    
    draw_element <- bias_sigma_list_IW[[sigma]][[draw_name]]
    
    # Identify training and test set by name pattern
    train_name <- names(draw_element)[grepl("^train", names(draw_element))]
    test_name  <- names(draw_element)[grepl("^test", names(draw_element))]
    
    train_df <- draw_element[[train_name]]
    test_df  <- draw_element[[test_name]]
    
    # Apply KLIEP estimation based on selected variables
    kliep_result <- estimate_kliep_pair(train_df, test_df, bias_vars)
    
    # Add importance weights to training data
    train_df$importance_weight <- kliep_result$importance_weights
    
    # Save updated training data back into list
    bias_sigma_list_IW[[sigma]][[draw_name]][[train_name]] <- train_df
    
    # Save KL divergence value
    bias_sigma_list_IW[[sigma]][[draw_name]]$kliep_value <- kliep_result$kliep_value
  }
}

#####

##### 5) Save bias_sigma_list_IW #####

saveRDS(bias_sigma_list_IW,
        file = "biased_data/bias_sigma_list_IW.rds")

#####