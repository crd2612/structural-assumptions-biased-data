
############################################################
# Script: B) Simulation under Covariate Shift (sigma = 1)
# Simulation study to compare global parametric and local nonparametric models 
# under covariate shift induced by a bias score
# (see Section 2.2.4 of the thesis)
############################################################


##### 1) Load packages and set seed #####

set.seed(42)

library(ggplot2)
library(randomForest)
library(gamlss)
library(reshape2)
library(dplyr)

#####

##### 2) Generate synthetic data (true function: sinusoidal + linear) #####

n <- 500
X <- rnorm(n, mean = 0, sd = 1)
f_X <- -1 * sin(2 * X) + 1 * X
epsilon <- rnorm(n, mean = 0, sd = 1)
Y <- f_X + epsilon

data_full <- data.frame(X = X, Y = Y)

#####

##### 3) Induce covariate shift in test data via bias score (sigma = 1) #####

sigma <- 1
X_z <- scale(data_full$X)
bias_score <- as.numeric(X_z) + rnorm(n, mean = 0, sd = sigma)

threshold <- quantile(bias_score, probs = 0.5)
test_idx <- which(bias_score >= threshold)
train_idx <- setdiff(1:n, test_idx)

train_data <- data_full[train_idx, ]
test_data  <- data_full[test_idx, ]

#####

##### 4) Estimate models on biased training data #####

model_lm  <- lm(Y ~ X, data = train_data)
model_gam <- gamlss(Y ~ pb(X), data = train_data, trace = FALSE)
model_rf  <- randomForest(Y ~ X, data = train_data, ntree = 500)

#####

##### 5) Predict on full grid (true function + all models) #####

X_grid <- seq(min(data_full$X), max(data_full$X), length.out = 300)
df_grid <- data.frame(X = X_grid)

df_grid$True <- -1 * sin(2 * X_grid) + 1 * X_grid
df_grid$LM   <- predict(model_lm, newdata = df_grid)
df_grid$GAM  <- predict(model_gam, newdata = data.frame(X = X_grid), type = "response")
df_grid$RF   <- predict(model_rf, newdata = df_grid)

#####

##### 6) Prepare data for plotting #####

df_long <- melt(df_grid, id.vars = "X", variable.name = "Model", value.name = "Prediction")

train_data$Group <- "Train"
test_data$Group  <- "Test"
combined_data <- rbind(train_data, test_data)

#####

##### 7) Plot 1: Model predictions vs. true function #####

model_colors <- c(
  "True" = "red",
  "LM"   = "blue",
  "GAM"  = "aquamarine",
  "RF"   = "violet"
)

ggplot() + 
  geom_line(data = subset(df_long, Model == "True"), aes(x = X, y = Prediction, color = "True"), size = 3, alpha = 0.8) +
  geom_line(data = subset(df_long, Model == "RF"),   aes(x = X, y = Prediction, color = "RF"),   size = 1.7) +
  geom_line(data = subset(df_long, Model == "LM"),   aes(x = X, y = Prediction, color = "LM"),   size = 1.7) +
  geom_line(data = subset(df_long, Model == "GAM"),  aes(x = X, y = Prediction, color = "GAM"),  size = 1.7) +
  geom_point(data = combined_data, aes(x = X, y = Y, fill = Group), 
             color = "black", size = 3, shape = 21, alpha = 0.55, stroke = 0.4) +
  scale_color_manual(name = "Model", values = model_colors) +
  scale_fill_manual(name = "Data Type", values = c("Train" = "lightblue", "Test" = "coral")) +
  scale_x_continuous(breaks = seq(floor(min(data_full$X)), ceiling(max(data_full$X)), by = 0.5)) +
  labs(y = "Y / Prediction", x = "X") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        plot.title = element_text(hjust = 0.5, size = 16))


#####

##### 8) Plot 2: Density distribution of X in train vs. test data #####

ggplot() + 
  geom_density(data = train_data, aes(x = X, fill = "Train", color = "Train"), alpha = 0.4, size = 0.8) +
  geom_density(data = test_data,  aes(x = X, fill = "Test",  color = "Test"),  alpha = 0.4, size = 0.8) +
  scale_fill_manual(name = "Sample", values = c("Train" = "lightblue", "Test" = "coral")) +
  scale_color_manual(name = "Sample", values = c("Train" = "white", "Test" = "white")) +
  labs(y = "Density", x = "X") +
  theme_minimal() +
  theme(legend.position = "right", 
        plot.title = element_text(hjust = 0.5, size = 16))


#####
