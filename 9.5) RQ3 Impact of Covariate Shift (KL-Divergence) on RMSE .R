
############################################################
# Script: 9.4) RQ3: Impact of Covariate Shift (KL-Divergence) on RMSE 
# This script investigates the relationship between distributional shift (measured via KL-divergence) 
# and predictive performance (RMSE) for parametric and nonparametric models across increasing bias levels. 
# It computes correlation statistics and visualizes both the KL-RMSE relationship and RMSE trends 
# across sigma levels for all models.
############################################################

##### 1) Load packages and model results #####

# Load required packages
library(ggplot2)
library(dplyr)

# Load model results with RMSE and KL divergence values
bias_sigma_list_result <- readRDS("biased_data/bias_sigma_list_result_without_IW.rds")

##### 

##### 2) Create combined KL–RMSE dataframe #####

# Extract RMSE and KL divergence for each model, draw, and sigma level
kl_rmse_list <- list()

for (sigma in names(bias_sigma_list_result)) {
  for (draw in names(bias_sigma_list_result[[sigma]])) {
    
    kl_value <- bias_sigma_list_result[[sigma]][[draw]]$kliep_value
    model_df <- bias_sigma_list_result[[sigma]][[draw]]$model_results %>%
      dplyr::select(Modell, RMSE)
    
    model_df$Sigma <- sigma
    model_df$Draw <- draw
    model_df$KL <- kl_value
    model_df <- model_df[, c("Sigma", "Draw", "KL", "Modell", "RMSE")]
    
    kl_rmse_list[[paste0("Sigma_", sigma, "_Draw_", draw)]] <- model_df
  }
}

# Combine all draws into a single dataframe
kl_rmse_df <- do.call(rbind, kl_rmse_list)

# Convert sigma level to numeric for plotting
kl_rmse_df$Sigma_num <- as.numeric(gsub("bias_sigma_", "", kl_rmse_df$Sigma))

##### 

##### 3) Assign model class (parametric vs. nonparametric) #####

# Add classification for each model to enable grouped analysis
kl_rmse_df$Modell_klasse <- dplyr::case_when(
  kl_rmse_df$Modell %in% c("RF", "XGB") ~ "Nonparametric",
  TRUE ~ "Parametric"
)
##### 

##### 4) Correlation analysis: KL divergence vs. RMSE #####

# Compute Pearson correlation between KL divergence and RMSE
# separately for parametric and nonparametric models

cor_param <- cor.test(kl_rmse_df$KL[kl_rmse_df$Modell_klasse == "Parametric"],
                      kl_rmse_df$RMSE[kl_rmse_df$Modell_klasse == "Parametric"],
                      method = "pearson")

cor_nonparam <- cor.test(kl_rmse_df$KL[kl_rmse_df$Modell_klasse == "Nonparametric"],
                         kl_rmse_df$RMSE[kl_rmse_df$Modell_klasse == "Nonparametric"],
                         method = "pearson")

# Print correlation results
cat("##### Pearson correlation KL ↔ RMSE → Parametric Models #####\n")
print(cor_param)

cat("##### Pearson correlation KL ↔ RMSE → Nonparametric Models #####\n")
print(cor_nonparam)
##### 

##### 5) Visualization: RMSE vs. KL divergence and model-specific RMSE trends #####

# Plot 1: Scatterplot of KL divergence vs. RMSE, colored by model class
# Includes linear trend lines to visualize correlation strength
ggplot(kl_rmse_df, aes(x = KL, y = RMSE, color = Modell_klasse)) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.5) +
  scale_color_manual(
    values = c("Parametric" = "lightblue", "Nonparametric" = "violet")
  ) +
  theme_minimal() +
  labs(
    x = "Kullback-Leibler Divergence (KLD)",
    y = "RMSE",
    color = "Model Class"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

# Plot 2: RMSE trends across sigma levels for each model, with variability bands
# Shows mean RMSE and ±1 SD bands for each model
rmse_summary <- kl_rmse_df %>%
  group_by(Sigma_num, Modell) %>%
  summarise(
    Mean_RMSE = mean(RMSE, na.rm = TRUE),
    SD_RMSE = sd(RMSE, na.rm = TRUE),
    .groups = "drop"
  )

# Set model order and colors
model_order <- c("LM_full", "LM_red", "GAMLSS_lin", "GAMLSS", "GAM", "RF", "XGB")
rmse_summary$Modell <- factor(rmse_summary$Modell, levels = model_order)

model_colors <- c(
  "LM_full"      = "blue",
  "LM_red"       = "cyan",
  "GAMLSS_lin"   = "darkslategrey",
  "GAMLSS"       = "aquamarine",
  "GAM"          = "darkseagreen",
  "RF"           = "pink",
  "XGB"          = "violet"
)

ggplot(rmse_summary, aes(x = Sigma_num, y = Mean_RMSE, color = Modell, fill = Modell)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = Mean_RMSE - SD_RMSE,
                  ymax = Mean_RMSE + SD_RMSE),
              alpha = 0.2, color = NA) +
  scale_color_manual(values = model_colors) +
  scale_fill_manual(values = model_colors) +
  scale_x_log10(breaks = c(0.1, 0.5, 1, 2, 5, 10, 25, 50, 100)) +
  theme_minimal() +
  labs(
    title = "RMSE across Sigma Levels with Variability Bands",
    x = expression(sigma ~ "(Bias Strength, log scale)"),
    y = "Mean RMSE",
    color = "Model",
    fill = "Model"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )
##### 

