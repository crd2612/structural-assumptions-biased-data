
##############################################################
# Script: 2) Imputation of Missing Values in tenant_df
#
# Description:
# This script performs a structured single imputation for the dataset `tenant_df`,
# which contains information on private tenants from the German Microcensus 2010.
# It begins by identifying variables with missing values and summarizing their patterns and types.
# Based on this overview, appropriate imputation methods are defined using the `mice` package.
# A custom predictor matrix is constructed to guide the imputation process, relying on domain
# knowledge and correlations among variables. The imputation is then carried out with one iteration (m = 1),
# and diagnostic plots are generated for key variables to check the plausibility and convergence of the results.
# Finally, the completed dataset is exported as `tenant_completed.rds` for subsequent analysis.
##############################################################


##### 1) Installing and loading required packages #####

library(dplyr)
library(corrplot)
library(mice)   
library(VIM)    
library(lattice)

#####

##### 2) Load data: tenant_df #####

tenant_df <- readRDS("data/tenant_df.rds")

#####

##### 3) Create copy for imputation #####

tenant_imputed <- tenant_df

#####

##### 4) Check missing data patterns #####

### Numeric pattern matrix of missing values
md.pattern(tenant_imputed)

### Graphical summary of missingness
aggr(
  tenant_imputed,
  numbers = TRUE,
  sortVars = TRUE,
  labels = names(tenant_imputed),
  cex.axis = 0.7,
  gap = 3,
  ylab = c("Missing data", "Pattern")
)

#####

##### 5) Overview of variables with missing values #####

### Count and summarize missing values
na_counts <- colSums(is.na(tenant_imputed))
na_counts <- na_counts[na_counts > 0]  

na_summary <- data.frame(
  Variable = names(na_counts),
  Missing_Count = as.vector(na_counts),
  Missing_Percent = round(na_counts / nrow(tenant_imputed) * 100, 2),
  Type = sapply(tenant_imputed[, names(na_counts)], function(x) class(x)[1])
)

print(na_summary)

### Identify factor variables with missing values
na_factors <- names(na_counts)[sapply(tenant_imputed[, names(na_counts)], is.factor)]

factor_summary <- data.frame(
  Variable = na_factors,
  Levels = sapply(tenant_imputed[, na_factors], nlevels),
  Level_Names = sapply(tenant_imputed[, na_factors], function(x) paste(levels(x), collapse = ", "))
)

print(factor_summary)

#####

##### 6) Define imputation methods for variables with missing values #####

### Initialize method vector based on variable types
method <- make.method(tenant_imputed)

### Explicitly define methods for variables with missing values
method["contract_type"]   <- "polyreg"   # categorical with >2 levels
method["rent_total"]      <- "pmm"       # numeric
method["rent_per_sqm"]    <- "pmm"       # numeric
method["rent_burden"]     <- "pmm"       # numeric
method["hh_net_income"]   <- "pmm"       # numeric
method["hh_head_income"]  <- "pmm"       # numeric

### Show selected methods
method[method != ""]

#####

##### 7) Custom predictorMatrix for improved imputation #####

### Correlation overview (numeric variables only)
numeric_data <- tenant_imputed %>%
  select(where(is.numeric)) %>%
  drop_na()  

cor_matrix <- cor(numeric_data)

corrplot(
  cor_matrix,
  method = "number",
  type = "upper",
  tl.cex = 0.7,
  tl.col = "black",
  number.cex = 0.5,
  col = colorRampPalette(c("blue", "white", "red"))(200),
  title = "Korrelationen numerischer Variablen",
  mar = c(0, 0, 2, 0)
)

### Define custom predictor matrix
pred_matrix <- make.predictorMatrix(tenant_imputed)
pred_matrix[,] <- 0  

### Specify relevant predictors per variable

# rent_total
pred_matrix["rent_total", c(
  "rent_per_sqm", "household_size", "hh_net_income",
  "rented_apartment", "building_size", "num_children_hh", "contract_type"
)] <- 1

# rent_per_sqm
pred_matrix["rent_per_sqm", c(
  "region", "building_type", "building_size", "rented_apartment",
  "hh_net_income", "contract_type"
)] <- 1

# rent_burden
pred_matrix["rent_burden", c(
  "rent_total", "hh_net_income", "region", "rented_apartment",
  "building_size", "household_size", "num_children_hh"
)] <- 1

# hh_net_income
pred_matrix["hh_net_income", c(
  "employment_type", "contract_type", 
  "worktime_type", "pension", "income_main_source",
  "hh_head_education", "hh_head_employment"
)] <- 1

# hh_head_income
pred_matrix["hh_head_income", c(
  "contract_type", "worktime_type",
  "pension", "income_main_source",
  "hh_head_education", "hh_head_employment", "gender", "age"
)] <- 1

### Optional: view custom matrix for selected variables
pred_matrix[rownames(pred_matrix) %in% c(
  "rent_total", "rent_per_sqm", "rent_burden",
  "hh_net_income", "hh_head_income"
), ]

#####

##### 8) Run MICE imputation with custom predictorMatrix #####

imp_custom <- mice(data = tenant_imputed,
                   method = method,
                   predictorMatrix = pred_matrix,
                   m = 1,
                   maxit = 10,
                   seed = 42,
                   printFlag = TRUE)

#####

##### 9) Imputation Diagnostics

##### 9.1) rent_burden #####

### Traceplot for convergence check
plot(imp_custom, "rent_burden")

### Density plot for distribution comparison
densityplot(imp_custom, ~ rent_burden)

#####

##### 9.2) contract_type #####

### Traceplot for convergence check
plot(imp_custom, "contract_type")

### Stripplot for categorical distribution comparison
stripplot(imp_custom, contract_type ~ .imp, pch = 20, cex = 1.2)

#####

##### 9.3) rent_total #####

### Traceplot for convergence check
plot(imp_custom, "rent_total")

### Density plot for distribution comparison
densityplot(imp_custom, ~ rent_total)

#####

##### 9.4) rent_per_sqm #####

### Traceplot for convergence check
plot(imp_custom, "rent_per_sqm")

### Density plot for distribution comparison
densityplot(imp_custom, ~ rent_per_sqm)

#####

##### 9.5) hh_net_income #####

### Traceplot for convergence check
plot(imp_custom, "hh_net_income")

### Density plot for distribution comparison
densityplot(imp_custom, ~ hh_net_income)

#####

##### 9.6) hh_head_income #####

### Traceplot for convergence check
plot(imp_custom, "hh_head_income")

### Density plot for distribution comparison
densityplot(imp_custom, ~ hh_head_income)

#####

##### 10) Create and inspect completed dataset #####

### Extract completed dataset (first imputation)
tenant_completed <- complete(imp_custom, action = 1)

### Check structure of completed data
str(tenant_completed)

#####

##### 11) Check completeness #####

### Compare variable names (original vs. completed)
names_completed <- colnames(tenant_completed)
names_original  <- colnames(tenant_df)

### Identify missing variables (if any)
variable_check <- setdiff(names_original, names_completed)
print(variable_check)

### Check for remaining NAs in the completed dataset
na_check_final <- sapply(tenant_completed, function(x) sum(is.na(x)))
print(na_check_final)

#####

##### 12) Save dataset  #####

saveRDS(tenant_completed, "data/tenant_completed.rds")

#####

