
##############################################################
# Script:5.1) Plot biased Variable rent_per_sqm
#
# Description:
# This script visualizes the induced bias across sigma levels based on the distribution
# of rent_per_sqm in training and test sets.
##############################################################

##### 1) Setup and Data Loading #####

### Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)

# Load data
bias_sigma_list <- readRDS("biased_data/bias_sigma_list.rds")

#####

##### 2) Prepare combined data for plotting #####

### Define variable to visualize
var_to_plot <- "rent_per_sqm"
viz_list <- list()

### Loop through all sigma levels and draws
for (sigma in names(bias_sigma_list)) {
  for (draw_name in names(bias_sigma_list[[sigma]])) {
    
    draw_element <- bias_sigma_list[[sigma]][[draw_name]]
    
    train_name <- names(draw_element)[grepl("^train", names(draw_element))]
    test_name  <- names(draw_element)[grepl("^test", names(draw_element))]
    
    ### Add metadata to train and test sets
    train_df <- draw_element[[train_name]] %>%
      mutate(Group = "Train", Draw = draw_name, Sigma = as.numeric(sigma))
    
    test_df <- draw_element[[test_name]] %>%
      mutate(Group = "Test", Draw = draw_name, Sigma = as.numeric(sigma))
    
    ### Combine and keep only relevant columns
    combined_df <- bind_rows(train_df, test_df) %>%
      select(all_of(var_to_plot), Group, Draw, Sigma)
    
    viz_list[[paste0(sigma, "_", draw_name)]] <- combined_df
  }
}

### Merge all into a single long-format dataframe
viz_df <- bind_rows(viz_list) %>%
  pivot_longer(cols = all_of(var_to_plot), names_to = "Variable", values_to = "Value")

### Reverse order of draws for better visual separation
viz_df$Draw <- factor(viz_df$Draw, levels = rev(paste0("Draw_", 1:5)))

### Define color palette for sigma levels
my_sigma_colors <- c(
  "#edf0f5", "#d7dde9", "#c1cada", "#acb7cb", "#96a4bc",
  "#8091ad", "#6a7e9e", "#556b8f", "#415980"
)

#####

##### 3) Final combined plot #####

ggplot(viz_df,
       aes(x = Value, y = Draw,
           fill = factor(Sigma),
           color = Group,
           linetype = Group,
           alpha = Group)) +
  geom_density_ridges(scale = 3, rel_min_height = 0.01, size = 0.4) +
  scale_fill_manual(values = my_sigma_colors) +
  scale_color_manual(values = c("Train" = "black", "Test" = "darkred")) +
  scale_linetype_manual(values = c("Train" = "solid", "Test" = "solid")) +
  scale_alpha_manual(values = c("Train" = 0.2, "Test" = 0.6)) +
  labs(
    title = "Density of Rent per Square Meter across Sigma Levels",
    x = "Rent per square meter (€)", 
    y = "",
    fill = "Sigma",
    color = "Group",
    linetype = "Group",
    alpha = "Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")

#####
