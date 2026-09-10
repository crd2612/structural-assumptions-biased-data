
############################################################
# Script: A) Comparison of Global and Local Models in a Nonlinear Setting 
# Simulation study to compare global parametric and local nonparametric models
# (see Section 2.2.4 of the thesis)
############################################################



##### 1) Load packages and set seed #####

# Load required packages
library(ggplot2)
library(randomForest)
library(gamlss)
library(reshape2)

# Set seed for reproducibility
set.seed(42)

#####

##### 2) Generate full dataset #####

# Define sample size
n <- 500

# Generate predictor X from standard normal distribution
X <- rnorm(n, mean = 0, sd = 1)

# Define true data-generating function
f_X <- -1 * sin(2 * X) + 1 * X

# Add Gaussian noise
epsilon <- rnorm(n, mean = 0, sd = 1)

# Generate response variable Y
Y <- f_X + epsilon

# Combine into full dataset
data_full <- data.frame(X = X, Y = Y)

#####


##### 3) Bias induction for test set selection #####

# Define bias strength (sigma)
sigma <- 100

# Standardize predictor X
X_z <- scale(data_full$X)

# Compute bias score: standardized X plus Gaussian noise
bias_score <- as.numeric(X_z) + rnorm(n, mean = 0, sd = sigma)

# Select top 50% as test set based on bias score
threshold <- quantile(bias_score, probs = 0.5)
test_idx <- which(bias_score >= threshold)
train_idx <- setdiff(1:n, test_idx)

# Split into biased training and test datasets
train_data <- data_full[train_idx, ]
test_data  <- data_full[test_idx, ]

#####
##### 4) Estimate models on biased training data #####

# Linear model (LM)
model_lm <- lm(Y ~ X, data = train_data)

# GAMLSS model with pb() smoother, stored as "GAM"
model_gam <- gamlss(Y ~ pb(X), data = train_data, trace = FALSE)

# Random Forest (RF)
model_rf <- randomForest(Y ~ X, data = train_data, ntree = 500)

#####

##### 5) Predict on full grid (true function + all models) #####

# Create fine grid over full X range
X_grid <- seq(min(data_full$X), max(data_full$X), length.out = 300)
df_grid <- data.frame(X = X_grid)

# Add true function and model predictions
df_grid$True <- -1 * sin(2 * X_grid) + 1 * X_grid
df_grid$LM   <- predict(model_lm, newdata = df_grid)
df_grid$GAM  <- predict(model_gam, newdata = data.frame(X = X_grid), type = "response")
df_grid$RF   <- predict(model_rf, newdata = df_grid)

#####

##### 6) Prepare data for plotting #####

# Convert to long format
df_long <- melt(df_grid, id.vars = "X", 
                variable.name = "Model", value.name = "Prediction")

# Combine train/test data for visualization
train_data$Group <- "Train"
test_data$Group <- "Test"
combined_data <- rbind(train_data, test_data)

#####

##### 7) Plot 1: Density distribution of training vs. test data #####

ggplot() + 
  geom_density(data = train_data, aes(x = X, fill = "Train", color = "Train"), alpha = 0.4, size = 0.8) +
  geom_density(data = test_data, aes(x = X, fill = "Test", color = "Test"), alpha = 0.4, size = 0.8) +
  scale_fill_manual(name = "Sample", values = c("Train" = "cornflowerblue", "Test" = "deeppink")) +
  scale_color_manual(name = "Sample", values = c("Train" = "white", "Test" = "white")) +
  labs(y = "Density", x = "X") +
  theme_minimal() +
  theme(legend.position = "right", 
        plot.title = element_text(hjust = 0.5, size = 16))

#####

##### 8) Plot 2: Model predictions vs. true function #####

# Define color palette
model_colors <- c(
  "LM"   = "blue",
  "GAM"  = "aquamarine",
  "RF"   = "violet",
  "True" = "red"
)

# Final plot
ggplot() + 
  geom_line(data = subset(df_long, Model == "True"), 
            aes(x = X, y = Prediction, color = "True"), size = 3, alpha = 0.8) +
  geom_line(data = subset(df_long, Model == "RF"), 
            aes(x = X, y = Prediction, color = "RF"), size = 1.7) +
  geom_line(data = subset(df_long, Model == "LM"), 
            aes(x = X, y = Prediction, color = "LM"), size = 1.7) +
  geom_line(data = subset(df_long, Model == "GAM"), 
            aes(x = X, y = Prediction, color = "GAM"), size = 1.7) +
  geom_point(data = combined_data, aes(x = X, y = Y, fill = Group), 
             color = "black", size = 2, shape = 21, alpha = 0.55, stroke = 0.4) +
  scale_color_manual(name = "Model", values = model_colors) +
  scale_fill_manual(name = "Data Type", values = c("Train" = "lightblue", "Test" = "coral")) +
  scale_x_continuous(breaks = seq(floor(min(data_full$X)), ceiling(max(data_full$X)), by = 0.5)) +
  labs(y = "Y / Prediction", x = "X") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        plot.title = element_text(hjust = 0.5, size = 16))

#####