
##############################################################
# Script: 3) Variable Selection for Bias Induction
#
# Description:
# This script selects suitable candidate variables for bias induction based on their correlation with the target variable `living_space`
# and additional criteria such as continuity and distribution shape. It supports the selection by visualizing the distributions
# and relationships of the chosen variables to the target.
##############################################################


##### 1) Setup and Data Loading #####

### Load required packages
library(dplyr)
library(ggplot2)
library(corrplot)

### Load completed dataset
tenant_completed <- readRDS("data/tenant_completed.rds")

#####

##### 2) Identify numeric variables (excluding target) #####

### Define target variable
target_var <- "living_space"

### Identify numeric predictors (excluding target)
numeric_vars <- tenant_completed %>%
  select(where(is.numeric)) %>%
  names()

numeric_predictors <- setdiff(numeric_vars, target_var)

#####

##### 3) Correlation matrix of top candidates #####

# Compute correlation between target and numeric predictors
correlation_df <- data.frame(
  Variable = numeric_predictors,
  Correlation = sapply(numeric_predictors, function(x) {
    cor(tenant_completed[[x]], tenant_completed[[target_var]], use = "complete.obs")
  })
)

# Sort by absolute correlation
correlation_df <- correlation_df %>%
  arrange(desc(abs(Correlation)))

### Select top 15 numeric variables by absolute correlation
top_vars <- correlation_df %>%
  top_n(15, wt = abs(Correlation)) %>%
  pull(Variable)

### Compute correlation matrix including target
cor_matrix <- tenant_completed %>%
  select(all_of(c(top_vars, target_var))) %>%
  cor(use = "complete.obs")

### Visualize correlation matrix
corrplot(
  cor_matrix,
  method = "number",
  type = "upper",
  tl.cex = 0.4
)

#####

##### 4) Selection of bias predictors #####

### Variable selection rationale:
# rent_total and hh_net_income are selected due to their strong correlation with the target variable (living_space).
# rent_per_sqm is chosen as a third candidate because, although some variables show even stronger correlations,
# they are either discrete (e.g., household_size) or highly collinear with already selected predictors 
# (e.g., hh_head_income is strongly correlated with hh_net_income).

bias_predictors <- c("rent_total", "hh_net_income", "rent_per_sqm")

#####




