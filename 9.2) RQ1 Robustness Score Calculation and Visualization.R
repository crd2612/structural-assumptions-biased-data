

############################################################
# Script: 9.2) RQ1: Robustness Score Calculation and Visualization
#
# This script computes robustness scores (Rf = RMSE_baseline / RMSE_biased)
# across different bias levels (sigma) and draws, visualizes the results,
# and conducts t-tests to compare parametric and nonparametric models.
############################################################


##### 1) Load packages, data, and set seed #####

# Load required packages
library(ggplot2)
library(dplyr)

# Load baseline RMSEs from 50:50 split
baseline_rmse <- readRDS("baseline_rmse.rds")

# Load model results from loop over all sigma levels and draws
bias_sigma_list_result <- readRDS("biased_data/bias_sigma_list_result_without_IW.rds")

# Set seed for reproducibility
set.seed(42)

#####

##### 2) Compute robustness scores R_f = RMSE_baseline / RMSE_bias #####

# Initialize empty list to store results
rf_results_list <- list()

# Loop over all sigma levels and draws
for (sigma in names(bias_sigma_list_result)) {
  for (draw in names(bias_sigma_list_result[[sigma]])) {
    
    # Extract RMSE results for current draw
    model_df <- bias_sigma_list_result[[sigma]][[draw]]$model_results
    
    # Merge with baseline RMSEs to compute robustness score
    merged_df <- merge(model_df[, c("Modell", "RMSE")], baseline_rmse, by = "Modell")
    merged_df$R_f <- merged_df$RMSE_baseline / merged_df$RMSE
    merged_df$Sigma <- sigma
    merged_df$Draw <- draw
    
    # Store in results list
    rf_results_list[[paste0("Sigma_", sigma, "_Draw_", draw)]] <- merged_df
  }
}

# Combine all results into a single data frame
rf_results_df <- do.call(rbind, rf_results_list)

# Extract numeric sigma value for plotting
rf_results_df$Sigma_num <- as.numeric(gsub("bias_sigma_", "", rf_results_df$Sigma))

#####

##### 3) Assign model class #####

# Classify models into Parametric and Nonparametric
rf_results_df$Model_Class <- dplyr::case_when(
  rf_results_df$Modell %in% c("RF", "XGB") ~ "Nonparametric",
  rf_results_df$Modell %in% c("GAMLSS", "GAMLSS_lin", "LM_full", "LM_red", "GAM") ~ "Parametric",
  TRUE ~ "Other"
)

#####

##### 4) Define model order, colors, and plot robustness scores #####

# Define order of models for legend and plotting
model_order <- c("LM_full", "LM_red", "GAMLSS_lin", "GAMLSS", "GAM", "RF", "XGB")
rf_results_df$Modell <- factor(rf_results_df$Modell, levels = model_order)

# Define custom color palette for models
model_colors <- c(
  "LM_full"      = "blue",           
  "LM_red"       = "cyan",           
  "GAMLSS_lin"   = "darkslategrey",  
  "GAMLSS"       = "aquamarine",     
  "GAM"          = "darkseagreen",   
  "RF"           = "pink",           
  "XGB"          = "violet"          
)

# Plot mean robustness scores over sigma levels
ggplot(rf_results_df, aes(x = Sigma_num, y = R_f, color = Modell, group = Modell)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2.5) +
  scale_x_log10(breaks = c(0.1, 0.5, 1, 2.5, 5, 10, 100, 1000, 10000)) +
  scale_color_manual(values = model_colors) +
  theme_minimal() +
  labs(
    x = expression(sigma ~ "(Bias Strength, log scale)"),
    y = expression(R[f]),
    color = "Model"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

#####

##### 5) Test significance of robustness score differences #####

# Aggregate R_f by model class, sigma, and draw
rf_agg_sigma <- rf_results_df %>%
  group_by(Model_Class, Sigma, Draw) %>%
  summarise(R_f_mean = mean(R_f, na.rm = TRUE), .groups = "drop")

# Prepare list to store t-test results
test_results_list <- list()

# Get unique sigma levels
sigma_levels <- unique(rf_agg_sigma$Sigma)

# Loop: perform t-test between Parametric and Nonparametric models per sigma
for (s in sigma_levels) {
  df_sub <- rf_agg_sigma %>% filter(Sigma == s)
  
  t_test <- t.test(R_f_mean ~ Model_Class, data = df_sub)
  
  test_results_list[[s]] <- data.frame(
    Sigma = s,
    t_statistic = t_test$statistic,
    df = t_test$parameter,
    p_value = t_test$p.value,
    mean_parametric = mean(df_sub$R_f_mean[df_sub$Model_Class == "Parametric"]),
    mean_nonparametric = mean(df_sub$R_f_mean[df_sub$Model_Class == "Nonparametric"])
  )
}

# Combine results into a single data frame
rf_ttest_sigma_df <- do.call(rbind, test_results_list)

# Sort by numeric sigma
rf_ttest_sigma_df <- rf_ttest_sigma_df %>%
  mutate(Sigma_num = as.numeric(gsub("bias_sigma_", "", Sigma))) %>%
  arrange(Sigma_num)

# Print summary
print(rf_ttest_sigma_df)

#####

