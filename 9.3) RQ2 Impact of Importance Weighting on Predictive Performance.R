
############################################################
# Script: 9.3) RQ2: Impact of Importance Weighting on Predictive Performance
# This script analyzes the impact of importance weighting (IW) on predictive performance.
# Specifically, it computes and visualizes the difference in RMSE (Delta-RMSE) between models
# trained with and without IW across various levels of covariate shift (bias_sigma).
############################################################


##### 1) Load packages and model results #####

# Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)

# Load model results WITHOUT importance weighting
bias_sigma_list_result <- readRDS("biased_data/bias_sigma_list_result_without_IW.rds")

# Load model results WITH importance weighting
bias_sigma_list_result_IW <- readRDS("biased_data/bias_sigma_list_result_with_IW.rds")
##### 

##### 2) Compute Delta-RMSE (No IW - With IW) #####

# Initialize empty list to store results
delta_rmse_list <- list()

# Loop over all sigma levels and draws
for (sigma in names(bias_sigma_list_result)) {
  for (draw in names(bias_sigma_list_result[[sigma]])) {
    
    # Ensure model results exist for both conditions
    if (!is.null(bias_sigma_list_result[[sigma]][[draw]]$model_results) &&
        !is.null(bias_sigma_list_result_IW[[sigma]][[draw]]$model_results)) {
      
      # Extract and rename RMSEs from models without IW
      model_df_noIW <- bias_sigma_list_result[[sigma]][[draw]]$model_results %>%
        select(Modell, RMSE) %>%
        rename(RMSE_noIW = RMSE)
      
      # Extract and rename RMSEs from models with IW
      model_df_IW <- bias_sigma_list_result_IW[[sigma]][[draw]]$model_results %>%
        mutate(Modell = gsub("_IW$", "", Modell)) %>%
        select(Modell, RMSE) %>%
        rename(RMSE_IW = RMSE)
      
      # Merge both RMSEs by model
      merged_df <- merge(model_df_noIW, model_df_IW, by = "Modell")
      
      # Calculate Delta-RMSE and store metadata
      if (nrow(merged_df) > 0) {
        merged_df$Delta_RMSE <- merged_df$RMSE_noIW - merged_df$RMSE_IW
        merged_df$Sigma <- sigma
        merged_df$Draw <- draw
        
        delta_rmse_list[[paste0("Sigma_", sigma, "_Draw_", draw)]] <- merged_df
      }
    }
  }
}

# Combine all results into one data frame
delta_rmse_df <- do.call(rbind, delta_rmse_list)

# Extract numeric sigma value from label
delta_rmse_df$Sigma_num <- as.numeric(gsub("bias_sigma_", "", delta_rmse_df$Sigma))

##### 




##### 4) Define model order and color palette #####

model_order <- c("LM_full", "LM_red", "GAMLSS_lin", "GAMLSS", "GAM", "RF", "XGB")
delta_rmse_df$Modell <- factor(delta_rmse_df$Modell, levels = model_order)

model_colors <- c(
  "LM_full"      = "blue",
  "LM_red"       = "cyan",
  "GAMLSS_lin"   = "darkslategrey",
  "GAMLSS"       = "aquamarine",
  "GAM"          = "darkseagreen",
  "RF"           = "pink",
  "XGB"          = "violet"
)

##### 5) Plot: Delta-RMSE by model (boxplot) #####

ggplot(delta_rmse_df, aes(x = Modell, y = Delta_RMSE, fill = Modell)) +
  geom_boxplot(alpha = 0.6) +
  scale_fill_manual(values = model_colors) +
  theme_minimal() +
  labs(
    title = "Improvement from Importance Weighting (Delta-RMSE)",
    x = "Model",
    y = "Delta-RMSE (no IW - with IW)"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

ggplot(delta_rmse_df, aes(x = Modell, y = Delta_RMSE, fill = Modell)) +
  geom_boxplot(alpha = 0.6) +
  scale_fill_manual(values = model_colors) +
  theme_minimal() +
  labs(
    x = "Model",
    y = expression(Delta * "RMSE (No IW - With IW)")
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text = element_text(size = 11)
  )


##### 6) Plot: Delta-RMSE by model across sigma levels #####

ggplot(delta_rmse_df, aes(x = Sigma_num, y = Delta_RMSE, color = Modell)) +
  geom_jitter(width = 1, height = 0.1, alpha = 0.7, size = 2) +
  scale_color_manual(values = model_colors) +
  theme_minimal() +
  labs(
    title = "Improvement from IW across Bias Levels",
    x = expression(sigma ~ "(Bias Strength)"),
    y = "Delta-RMSE (no IW - with IW)",
    color = "Model"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

##### 7) Plot: Faceted boxplots by sigma #####

ggplot(delta_rmse_df, aes(x = Modell, y = Delta_RMSE, fill = Modell)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~ Sigma_num, scales = "free") +
  scale_fill_manual(values = model_colors) +
  theme_minimal() +
  labs(
    title = "Delta-RMSE by Model (Faceted by Bias Level)",
    x = "Model",
    y = "Delta-RMSE"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )


