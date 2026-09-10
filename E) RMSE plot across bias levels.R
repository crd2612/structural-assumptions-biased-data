
############################################################
# Script: D) RMSE plot across bias levels
#
# RMSE plot across bias levels (log-scale) 
# showing mean and standard deviation per model
############################################################

##### 1) Setup #####

# Load required packages
library(dplyr)
library(ggplot2)
library(tidyr)

# Load model results
bias_sigma_list_result <- readRDS("biased_data/bias_sigma_list_result_without_IW.rds")

# Without Importance Weighting:
# bias_sigma_list_result <- readRDS("biased_data/bias_sigma_list_result_without_IW.rds")

# Helper function to extract RMSE values
extract_rmse <- function(result_list) {
  res_all <- list()
  for (sigma in names(result_list)) {
    for (draw in names(result_list[[sigma]])) {
      df <- result_list[[sigma]][[draw]]$model_results
      if (!is.null(df)) {
        df$Sigma <- sigma
        df$Draw <- draw
        res_all[[paste(sigma, draw, sep = "_")]] <- df
      }
    }
  }
  do.call(rbind, res_all)
}

#####

##### 2) RMSE Summary and Plot #####

# Create RMSE dataframe from result list
rmse_df <- extract_rmse(bias_sigma_list_result)

# Convert Sigma to numeric (ensure dot as decimal separator)
rmse_df$Sigma_num <- as.numeric(gsub(",", ".", rmse_df$Sigma))

# Compute mean and standard deviation of RMSE per model and bias level
rmse_summary <- rmse_df %>%
  group_by(Sigma_num, Modell) %>%
  summarise(
    Mean_RMSE = mean(RMSE, na.rm = TRUE),
    SD_RMSE = sd(RMSE, na.rm = TRUE),
    .groups = "drop"
  )

# Define model order and assign colors
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

# Final RMSE plot (log-scaled x-axis)
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
    x = expression(sigma ~ "(Bias Strength, log scale)"),
    y = "Mean RMSE",
    color = "Model",
    fill = "Model"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13)
  )

#####
