
############################################################
# Script: C) Challenges of Importance Weighting
#
# Illustration of challenges of importance weighting
# including ESS calculation
# (see Section 3.3.3 of the thesis)
############################################################

##### 1) Settings & Packages #####

# Load required package
library(ggplot2)

# Set seed for reproducibility
set.seed(42)

#####

##### 2) Data Generation (as in Section 2.3) #####

# Sample size
n <- 500

# Generate covariate X
X <- rnorm(n, mean = 0, sd = 1)

# True nonlinear function
f_X <- -1 * sin(2 * X) + 1 * X

# Add Gaussian noise
epsilon <- rnorm(n, mean = 0, sd = 1)

# Generate response variable
Y <- f_X + epsilon

# Full dataset
data_full <- data.frame(X = X, Y = Y)

#####

##### 3) Bias Induction via Bias Score #####

# Set bias strength: sigma = 0.5 or 1 for moderate shift
sigma <- 0.5  # or 1 for moderate shift

# Standardize X
X_z <- scale(X)

# Compute bias score (standardized X + Gaussian noise)
bias_score <- as.numeric(X_z) + rnorm(n, mean = 0, sd = sigma)

# Split into train/test based on top 50% of bias score
threshold <- quantile(bias_score, probs = 0.5)
test_idx  <- which(bias_score >= threshold)
train_idx <- setdiff(1:n, test_idx)

# Create training and test sets
train_data <- data_full[train_idx, ]
test_data  <- data_full[test_idx, ]

#####

##### 4) Visualize Covariate Shift #####

ggplot() + 
  geom_density(data = train_data, aes(x = X, fill = "Train"), alpha = 0.5) +
  geom_density(data = test_data, aes(x = X, fill = "Test"), alpha = 0.5) +
  scale_fill_manual(values = c("Train" = "skyblue", "Test" = "tomato")) +
  labs(title = "Covariate Shift in X (Bias Score)", x = "X", y = "Density") +
  theme_minimal()

#####

##### 5) Estimate Importance Weights via KDE #####

# Kernel density estimates for training and test set
dens_train <- density(train_data$X)
dens_test  <- density(test_data$X)

# Evaluate both densities at training points
train_density_est <- approx(dens_train$x, dens_train$y, xout = train_data$X, rule = 2)$y
test_density_est  <- approx(dens_test$x,  dens_test$y,  xout = train_data$X, rule = 2)$y

# Compute importance weights as ratio of test to train density
weights_kde <- test_density_est / train_density_est

# Inspect weight distribution
summary(weights_kde)

#####

##### 6) Visualize Importance Weights #####

# Scatterplot of weights across X
ggplot(data.frame(X = train_data$X, w = weights_kde), aes(x = X, y = w)) +
  geom_point(color = "blue", size = 1.5) +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  labs(
    title = "Importance Weights (KDE, evaluated on training data)",
    x = "X (Train Data)",
    y = "w(X)"
  ) +
  theme_minimal()

#####

##### 7) Clean Thesis-Style Combined Plot #####

### Prepare density and weight data
density_train_df <- data.frame(X = dens_train$x, Density = dens_train$y, Group = "Train")
density_test_df  <- data.frame(X = dens_test$x,  Density = dens_test$y,  Group = "Test")
density_df       <- rbind(density_train_df, density_test_df)
weights_df       <- data.frame(X = train_data$X, w = weights_kde)

### Rescale weights to match density scale
scale_factor <- max(density_df$Density) / max(weights_kde)

### Compute and format Effective Sample Size
ESS <- (sum(weights_kde))^2 / sum(weights_kde^2)
ESS_label <- paste0("ESS = ", round(ESS, 1))

### Plot densities and weights (dual y-axis)
ggplot() +
  geom_area(data = density_train_df, aes(x = X, y = Density, fill = "Train"), alpha = 0.4) +
  geom_area(data = density_test_df,  aes(x = X, y = Density, fill = "Test"),  alpha = 0.4) +
  geom_point(data = weights_df, aes(x = X, y = w * scale_factor),
             color = "black", size = 1.5, alpha = 0.7) +
  scale_fill_manual(name = "Group", values = c("Train" = "lightblue", "Test" = "coral")) +
  scale_y_continuous(
    name = "Density",
    sec.axis = sec_axis(~ . / scale_factor, name = "Importance Weights")
  ) +
  labs(x = "X") +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "black"),
    axis.title.y.left  = element_text(color = "black"),
    legend.position = "bottom"
  )

#####

##### 8) Effective Sample Size #####

# Final ESS output
ESS

#####
