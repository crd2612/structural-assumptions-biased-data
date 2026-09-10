
############################################################
# Script: D) KS-Test for Detection of Covariate Shift
# 
# Statistical test to detect distributional differences 
# between train and test sets across sigma levels 
############################################################

##### 1) Setup #####

# Load required package
library(dplyr)
library(tidyr)

# Load result list (contains train/test splits per sigma and draw)
bias_sigma_list <- readRDS("biased_data/bias_sigma_list_result_without_IW.rds")

# Define variables to be tested for distributional shift
bias_vars <- c("rent_per_sqm", "hh_net_income", "rent_total")

# Initialize empty list to store KS test results
ks_results_list <- list()

#####

##### 2) Loop over all sigma levels and draws #####

for (sigma in names(bias_sigma_list)) {
  
  sigma_results <- list()
  
  for (draw_name in names(bias_sigma_list[[sigma]])) {
    
    draw_element <- bias_sigma_list[[sigma]][[draw_name]]
    
    # Identify train and test dataset names
    train_name <- names(draw_element)[grepl("^train", names(draw_element))]
    test_name  <- names(draw_element)[grepl("^test", names(draw_element))]
    
    train_df <- draw_element[[train_name]]
    test_df  <- draw_element[[test_name]]
    
    # Compute KS p-values for all bias variables
    pvals <- sapply(bias_vars, function(var) {
      ks_result <- ks.test(train_df[[var]], test_df[[var]])
      return(ks_result$p.value)
    })
    
    # Store p-values for this draw
    sigma_results[[draw_name]] <- pvals
  }
  
  # Store results per sigma level
  ks_results_list[[sigma]] <- sigma_results
}

#####

##### 3) Extract and summarize KS p-values across all draws #####

# Convert nested list into long-format data frame
results_long <- data.frame()

for (sigma in names(ks_results_list)) {
  for (draw in names(ks_results_list[[sigma]])) {
    pvals <- ks_results_list[[sigma]][[draw]]
    temp_df <- data.frame(
      Sigma    = as.numeric(sigma),
      Draw     = draw,
      Variable = names(pvals),
      P_Value  = as.numeric(pvals)
    )
    results_long <- rbind(results_long, temp_df)
  }
}

# Compute average KS p-value per variable and sigma level
mean_pvals <- results_long %>%
  group_by(Sigma, Variable) %>%
  summarise(Mean_P_Value = mean(P_Value, na.rm = TRUE)) %>%
  ungroup()

# Print table with mean p-values
print(mean_pvals)

#####

##### 4) Compute overall mean KS p-values per Sigma level #####

# Convert to wide format for summary table
mean_pvals_wide <- pivot_wider(mean_pvals, 
                               names_from = Variable, 
                               values_from = Mean_P_Value)

# Add overall average p-value across all variables
mean_pvals_wide <- mean_pvals_wide %>%
  mutate(Overall_Mean_P = rowMeans(select(., rent_total, hh_net_income, rent_per_sqm), na.rm = TRUE)) %>%
  arrange(Sigma)

# Print final summary table
print(mean_pvals_wide)

#####
