##############################################################
# Script: 5.3) Plot biased variable rent_total
#
# Description:
# This script visualizes the induced bias across sigma levels
# based on the relative frequency distribution of rent_total
# in training and test sets.
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
var_to_plot <- "rent_total"
viz_list_all <- list()

### Loop through all sigma levels and draws
for (sigma in names(bias_sigma_list)) {
  for (draw_name in names(bias_sigma_list[[sigma]])) {
    
    draw_element <- bias_sigma_list[[sigma]][[draw_name]]
    
    train_name <- names(draw_element)[grepl("^train", names(draw_element))]
    test_name  <- names(draw_element)[grepl("^test", names(draw_element))]
    
    ### Count relative frequencies for each category
    train_df <- draw_element[[train_name]] %>%
      mutate(Group = "Train", Sigma = as.numeric(sigma), Draw = draw_name) %>%
      count(Group, Sigma, Draw, !!sym(var_to_plot)) %>%
      group_by(Group, Sigma, Draw) %>%
      mutate(prop = n / sum(n)) %>%
      rename(Category = !!sym(var_to_plot))
    
    test_df <- draw_element[[test_name]] %>%
      mutate(Group = "Test", Sigma = as.numeric(sigma), Draw = draw_name) %>%
      count(Group, Sigma, Draw, !!sym(var_to_plot)) %>%
      group_by(Group, Sigma, Draw) %>%
      mutate(prop = n / sum(n)) %>%
      rename(Category = !!sym(var_to_plot))
    
    viz_list_all[[paste0(sigma, "_", draw_name)]] <- bind_rows(train_df, test_df)
  }
}

### Combine into long-format data frame
viz_df_all <- bind_rows(viz_list_all) %>%
  mutate(
    Category = as.numeric(as.character(Category)),
    Draw = factor(Draw, levels = rev(paste0("Draw_", 1:5)))  # Draw_1 at top
  )

### Define colors
my_sigma_colors <- c(
  "#edf0f5", "#d7dde9", "#c1cada", "#acb7cb", "#96a4bc",
  "#8091ad", "#6a7e9e", "#556b8f", "#415980"
)

group_colors <- c("Train" = "black", "Test" = "darkred")
scale_factor <- 4  # Height of areas relative to draw spacing

#####

##### 3) Final combined plot #####

ggplot(viz_df_all,
       aes(x = Category,
           group = interaction(Sigma, Group, Draw))) +
  geom_ribbon(aes(ymin = as.numeric(Draw),
                  ymax = as.numeric(Draw) + prop * scale_factor,
                  fill = factor(Sigma)),
              alpha = 0.5, color = NA) +
  geom_line(aes(y = as.numeric(Draw) + prop * scale_factor,
                color = Group),
            size = 0.4) +
  scale_fill_manual(values = my_sigma_colors) +
  scale_color_manual(values = group_colors) +
  scale_y_continuous(breaks = 1:5, labels = paste0("Draw_", 6 - 1:5)) +
  labs(
    title = "Relative Frequencies of Total Rent across Sigma Levels",
    x = "Total rent",
    y = "",
    fill = "Sigma",
    color = "Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")

#####
