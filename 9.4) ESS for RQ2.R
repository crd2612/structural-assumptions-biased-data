
############################################################
# Script: 9.5) ESS for RQ2
#
# This script computes the Effective Sample Size (ESS) for each draw and sigma level
# based on importance weights and visualizes the relationship between ESS and model performance.
############################################################


##### 1) Setup #####

# Load required packages
library(dplyr)
library(ggplot2)
library(tidyr)

# Load result list with importance weights 
bias_sigma_list_result <- readRDS("biased_data/bias_sigma_list_result_with_IW.rds")

# Define ESS calculation
calculate_ess <- function(w) {
  sum_w <- sum(w)
  ess <- (sum_w^2) / sum(w^2)
  return(ess)
}
#####

##### 2) Compute ESS and store in result list and dataframe #####

# Create empty data frame to collect ESS values for plotting
ess_df <- data.frame(
  sigma = character(),
  draw = character(),
  ESS = numeric(),
  stringsAsFactors = FALSE
)

# Loop over all sigma values and draws
for (sigma in names(bias_sigma_list_result)) {
  for (draw in names(bias_sigma_list_result[[sigma]])) {
    
    draw_element <- bias_sigma_list_result[[sigma]][[draw]]
    
    # Identify training data
    train_name <- names(draw_element)[grepl("^train", names(draw_element))]
    train_df <- draw_element[[train_name]]
    
    # If weights exist, compute ESS
    if (!is.null(train_df$importance_weight)) {
      ess <- calculate_ess(train_df$importance_weight)
    } else {
      ess <- NA
    }
    
    # Save ESS in list
    bias_sigma_list_result[[sigma]][[draw]]$ESS <- ess
    
    # Save for plotting
    ess_df <- rbind(ess_df, data.frame(
      sigma = sigma,
      draw = draw,
      ESS = ess
    ))
  }
}
#####

##### 3) Visualization of Effective Sample Size (ESS) #####

# Ensure correct factor order for sigma
ess_df$sigma <- factor(ess_df$sigma, levels = names(bias_sigma_list_result))

# 3.1) Boxplot: Distribution of ESS per Sigma
ggplot(ess_df, aes(x = sigma, y = ESS)) +
  geom_boxplot(
    width = 0.5,
    fill = "#bdd7e7",
    color = "black",
    alpha = 0.6
  ) +
  labs(
    x = expression(sigma ~ "(Bias Strength)"),
    y = "Effective Sample Size (ESS)"
  ) +
  theme_minimal(base_size = 13)

# 3.2) Scatterplot: RMSE vs. ESS by Model Type

# Define model types
parametric_models <- c("LM_full", "LM_red", "GAMLSS", "GAMLSS_lin", "GAM")
nonparametric_models <- c("RF", "XGB")

# Extract RMSE and ESS for relevant models
rmse_ess_list <- list()

for (sigma in names(bias_sigma_list_result)) {
  for (draw in names(bias_sigma_list_result[[sigma]])) {
    
    res <- bias_sigma_list_result[[sigma]][[draw]]
    
    if (!is.null(res$model_results) && !is.null(res$ESS)) {
      rmse_df <- res$model_results %>%
        select(Modell, RMSE) %>%
        mutate(
          Sigma = as.numeric(sigma),
          Draw = draw,
          ESS = res$ESS,
          Model_Type = case_when(
            Modell %in% parametric_models     ~ "Parametric",
            Modell %in% nonparametric_models  ~ "Nonparametric",
            TRUE ~ NA_character_
          )
        ) %>%
        filter(!is.na(Model_Type))
      
      rmse_ess_list[[paste(sigma, draw)]] <- rmse_df
    }
  }
}

rmse_ess_df <- bind_rows(rmse_ess_list)

# Plot: RMSE vs. ESS
ggplot(rmse_ess_df, aes(x = ESS, y = RMSE, color = Model_Type)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(
    x = "Effective Sample Size (ESS)",
    y = "Root Mean Squared Error (RMSE)",
    color = "Model Type"
  ) +
  theme_minimal(base_size = 13)

#####
