
##############################################################
# Script: 4) Bias Induction 
#
# Description:
# This script creates biased training and test splits by selecting observations based on a score
# computed from standardized predictors and added Gaussian noise. For multiple sigma levels and repeated draws,
# the top-scoring observations are assigned to the test set to simulate covariate shift.
# All resulting train/test splits are stored in a nested list (bias_sigma_list) and saved for further use.
##############################################################

##### 1) Setup and Data Loading #####

### Load required packages
library(dplyr)
library(ggplot2)
library(tidyr)

### Load data 
tenant_completed <- readRDS("data/tenant_completed.rds")

### Set seed for reproducibility
set.seed(42)

#####

##### 2) Define parameters for bias induction #####

### Define size of test set (top 50% based on score = 4523 of 9046 observations)
n_test <- 4523

### Define variables used for score calculation (selected in Step 3 of variable selection script)
bias_vars  <- c("rent_total", "hh_net_income", "rent_per_sqm")
target_var <- "living_space"

### Define bias strength levels (standard deviation of noise)
sigma_vector <- c(0.1, 0.5, 1, 2, 5, 10, 25, 50, 100)

### Number of repeated draws per sigma level
n_draws <- 5

### Initialize output list
bias_sigma_list <- list()

#####

##### 3) Bias induction loop for multiple sigma levels #####

for (sigma_value in sigma_vector) {
  
  sigma_list <- list()  # Temporary list for this sigma level
  
  for (i in 1:n_draws) {
    
    ### Standardize bias predictors
    score_data <- tenant_completed %>%
      mutate(
        rent_total_z    = scale(rent_total)[, 1],
        hh_net_income_z = scale(hh_net_income)[, 1],
        rent_burden_z   = scale(rent_burden)[, 1]  
      )
    
    ### Extract standardized predictors for score calculation
    z_data <- score_data %>% select(ends_with("_z"))
    colnames(z_data) <- paste0(bias_vars, "_bias")
    
    ### Generate Noise and compute score
    epsilon <- rnorm(nrow(z_data), mean = 0, sd = sigma_value)
    score   <- rowSums(z_data) + epsilon
    
    ### Select top n_test observations based on score
    selected_test_indices <- order(score, decreasing = TRUE)[1:n_test]
    
    test_data  <- tenant_completed[selected_test_indices, ]
    train_data <- tenant_completed[-selected_test_indices, ]
    
    ### Store train and test split for this draw
    draw_list <- list(
      train_data,
      test_data
    )
    
    names(draw_list) <- c(
      paste0("train_", sigma_value, "_", i),
      paste0("test_", sigma_value, "_", i)
    )
    
    sigma_list[[paste0("Draw_", i)]] <- draw_list
  }
  
  ### Store all draws for this sigma level
  bias_sigma_list[[as.character(sigma_value)]] <- sigma_list
}

#####

##### 4) Save full bias_sigma_list #####

### Save full list of biased train/test splits
saveRDS(
  bias_sigma_list,
  file = "biased_data/bias_sigma_list.rds"
)

#####

