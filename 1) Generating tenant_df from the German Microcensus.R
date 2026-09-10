

##############################################################
# Script: 1) Generating tenant_df from the German Microcensus
#
# Purpose: Prepare the tenant_df dataset from the German Microcensus 2010
#
# Description:
# This script loads the full Microcensus dataset,
# selects relevant variables, renames them, filters the data 
# for main tenants (EF491 == 3), and prepares the resulting 
# tenant_df for analysis. The resulting dataset includes 
# socio-demographic, housing, and income variables relevant 
# for modeling rental housing conditions in Germany.
#
# Output:
# - Cleaned RDS file: data/tenant_df.rds
##############################################################


##### 1) Installing and loading required packages #####

library(haven)
library(dplyr)
library(tidyr)

#####

##### 2) Load Mikrozensus data #####

### Source: German Microcensus 2010 (Campus File)
## Download: https://campus-file-fdz.nrw.de/
## Format: SPSS (.sav)

# Read full dataset
# The Microcensus data cannot be included in this repository.
# Please obtain the 2010 Microcensus Campus File and adjust the path below.
MZ_full <- read_sav("PATH_TO_MICROCENSUS_DATA/mz2010_cf.sav")

#####

##### 3) Select and rename relevant variables #####

### Define vector of selected EF variables
## EF492 (living_space) is the target variable

selected_vars <- c(
  "EF1", "EF29", "EF33", "EF44", "EF46", "EF49", "EF75",
  "EF124", "EF129",  "EF160", "EF231", "EF368", "EF401", 
  "EF402",  "EF431", "EF433", "EF489", "EF491", "EF492", 
  "EF495",  "EF504", "EF570", "EF616", "EF638", "EF639", 
  "EF664", "EF668", "EF669",  "EF707", "EF731", "EF734", 
  "EF735", "EF736", "EF741", "EF742",  "EF743", "EF746", 
  "EF865"
)

### Subset dataset to selected variables
MZ_sub <- MZ_full[, selected_vars]

### Rename EF variables to descriptive names
MZ_sub <- MZ_sub %>%
  rename(
    region = EF1,
    employment_type = EF29,
    partner_status = EF33,
    age = EF44,
    gender = EF46,
    marital_status = EF49,
    second_home = EF75,
    contract_type = EF124,
    worktime_type = EF129,
    minijob = EF160,
    job_search = EF231,
    nationality = EF368,
    income_main_source = EF401,
    pension = EF402,
    income_assets = EF431,
    income_rent = EF433,
    building_type = EF489,
    housing_status = EF491,
    living_space = EF492,       # target variable
    rented_apartment = EF495,
    rent_total = EF504,
    building_size = EF570,
    household_size = EF616,
    rent_per_sqm = EF638,
    rent_burden = EF639,
    num_employed_hh = EF664,
    num_foreigners_hh = EF668,
    num_children_hh = EF669,
    hh_net_income = EF707,
    hh_head_gender = EF731,
    hh_head_nationality = EF734,
    hh_head_marital_status = EF735,
    hh_head_employment = EF736,
    hh_head_income_source = EF741,
    hh_head_income = EF742,
    hh_head_education = EF743,
    hh_head_residence = EF746,
    family_type = EF865
  )

#####

##### 4) Filter for main tenants (value = 3) #####

### EF491: Housing status
## 1 = Owner of building
## 2 = Owner of apartment
## 3 = Main tenant           → keep only this group
## 4 = Subtenant
## 9 = No information
## NA = institutional / communal housing

### Keep only main tenants and drop all other groups
tenant_df <- MZ_sub %>%
  filter(housing_status == 3) %>%     
  filter(!is.na(housing_status)) %>%  
  select(-housing_status)             

#####

##### 5) Check variables #####

##### 5.1) region - EF1 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$region <- as.numeric(tenant_df$region)

### Recode to binary dummy:
## 1 = West (EF1 == 1)
## 0 = East (EF1 == 11)
## NA = all other values
tenant_df$region <- ifelse(tenant_df$region == 1, 1,
                           ifelse(tenant_df$region == 11, 0, NA))

### Check for missing values and distribution
sum(is.na(tenant_df$region))
table(tenant_df$region)

#####

##### 5.2) employment_type - EF29 #####

### Check for missing values and distribution
sum(is.na(tenant_df$employment_type))
table(tenant_df$employment_type)

### Recode to labeled factor
tenant_df$employment_type <- factor(tenant_df$employment_type,
                                    levels = c(1, 2, 3, 4),
                                    labels = c("Employed",
                                               "Unemployed",
                                               "Job-seeking inactive",
                                               "Other inactive"))

### Verify recoding
table(tenant_df$employment_type)

#####

##### 5.3) age - EF44 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$age <- as.numeric(tenant_df$age)

### Remove individuals under 18 (only adults relevant for housing context)
tenant_df <- tenant_df %>% filter(age >= 18)

### Check for missing values and distribution
sum(is.na(tenant_df$age))
table(tenant_df$age)

#####

##### 5.4) partner_status - EF33 #####

### Check for missing values and distribution
sum(is.na(tenant_df$partner_status))
table(tenant_df$partner_status)

### Recode to labeled factor:
## 1 = married
## 2 = cohabiting
## NA = no partner in household (set to "no_partner")
tenant_df <- tenant_df %>% 
  mutate(partner_status = case_when(
    partner_status == 1 ~ "married",
    partner_status == 2 ~ "cohabiting",
    is.na(partner_status) ~ "no_partner"
  ))

tenant_df$partner_status <- factor(tenant_df$partner_status,
                                   levels = c("married", "cohabiting", "no_partner"))

### Verify recoding
table(tenant_df$partner_status)

#####

##### 5.5) gender - EF46 #####

### Check for missing values and distribution
sum(is.na(tenant_df$gender))
table(tenant_df$gender)

### Recode to binary dummy:
## 1 = male → 1
## 2 = female → 0
## others/NA → NA
tenant_df <- tenant_df %>%
  mutate(gender = ifelse(gender == 1, 1,
                         ifelse(gender == 2, 0, NA)))

### Verify recoding
table(tenant_df$gender)

#####

##### 5.6) marital_status - EF49 #####

### Check for missing values and distribution
sum(is.na(tenant_df$marital_status))
table(tenant_df$marital_status)

### Recode to labeled factor:
## Combine both civil union categories into one
tenant_df$marital_status <- factor(tenant_df$marital_status,
                                   levels = c(1, 2, 3, 4, 5, 6),
                                   labels = c("single",
                                              "married",
                                              "widowed",
                                              "divorced",
                                              "civil_union",
                                              "civil_union"))

### Verify recoding
table(tenant_df$marital_status)

#####

##### 5.7) second_home - EF75 #####

### Check for missing values and distribution
sum(is.na(tenant_df$second_home))
table(tenant_df$second_home)

### Recode to binary dummy:
## 1 = second home → 1
## 8 = no second home → 0
## others/NA → NA
tenant_df$second_home <- ifelse(tenant_df$second_home == 1, 1,
                                ifelse(tenant_df$second_home == 8, 0, NA))

### Verify recoding
table(tenant_df$second_home)

#####

##### 5.8) contract_type - EF124 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$contract_type <- as.numeric(tenant_df$contract_type)

### Save original NAs (representing non-working individuals)
initial_NAs <- is.na(tenant_df$contract_type)

### Recode to labeled factor:
## 1 = fixed_term, 2 = permanent, 3 = self_employed
## 9 = NA (missing), original NA = unemployed
tenant_df$contract_type <- na_if(tenant_df$contract_type, 9)

tenant_df$contract_type <- case_when(
  tenant_df$contract_type == 1 ~ "fixed_term",
  tenant_df$contract_type == 2 ~ "permanent",
  tenant_df$contract_type == 3 ~ "self_employed",
  initial_NAs ~ "unemployed",
  TRUE ~ NA_character_
)

tenant_df$contract_type <- factor(tenant_df$contract_type,
                                  levels = c("fixed_term", "permanent", "self_employed", "unemployed"))

### Verify recoding
table(tenant_df$contract_type, useNA = "ifany")

#####

##### 5.9) worktime_type - EF129 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$worktime_type <- as.numeric(tenant_df$worktime_type)

### Recode to labeled factor:
## 1 = full_time
## 2 = part_time
## NA = unemployed
tenant_df$worktime_type <- case_when(
  tenant_df$worktime_type == 1 ~ "full_time",
  tenant_df$worktime_type == 2 ~ "part_time",
  is.na(tenant_df$worktime_type) ~ "unemployed"
)

tenant_df$worktime_type <- factor(tenant_df$worktime_type,
                                  levels = c("full_time", "part_time", "unemployed"),
                                  labels = c("Full-time", "Part-time", "Unemployed"))

### Verify recoding
table(tenant_df$worktime_type)

#####

##### 5.10) minijob - EF160 #####

### Check for missing values and distribution
sum(is.na(tenant_df$minijob))
table(tenant_df$minijob, useNA = "ifany")

### Recode to labeled factor:
## 1, 2, 3 = minijob
## 8 = kein_minijob
## NA = unemployed
tenant_df <- tenant_df %>%
  mutate(minijob = case_when(
    minijob %in% c(1, 2, 3) ~ "minijob",
    minijob == 8 ~ "kein_minijob",
    is.na(minijob) ~ "unemployed"
  ))

tenant_df$minijob <- factor(tenant_df$minijob,
                            levels = c("minijob", "kein_minijob", "unemployed"))

### Verify recoding
table(tenant_df$minijob)

#####

##### 5.11) job_search - EF231 #####

### Check for missing values and distribution
sum(is.na(tenant_df$job_search))
table(tenant_df$job_search, useNA = "ifany")

### Recode to labeled factor:
## 1 = searching
## 8 = not_searching
## NA = employed
tenant_df$job_search <- case_when(
  tenant_df$job_search == 1 ~ "searching",
  tenant_df$job_search == 8 ~ "not_searching",
  is.na(tenant_df$job_search) ~ "employed"
)

tenant_df$job_search <- factor(tenant_df$job_search,
                               levels = c("searching", "not_searching", "employed"))

### Verify recoding
table(tenant_df$job_search)

#####

##### 5.12) nationality - EF368 #####

### Check for missing values and distribution
sum(is.na(tenant_df$nationality))
table(tenant_df$nationality)

### Recode to binary dummy:
## 1 = German → 1
## 2 = Foreign → 0
## others/NA → NA
tenant_df$nationality <- ifelse(tenant_df$nationality == 1, 1,
                                ifelse(tenant_df$nationality == 2, 0, NA))

### Verify recoding
table(tenant_df$nationality, useNA = "ifany")

#####

##### 5.13) income_main_source - EF401 #####

### Check for missing values and distribution
sum(is.na(tenant_df$income_main_source))
table(tenant_df$income_main_source)

### Recode to labeled factor:
tenant_df$income_main_source <- factor(tenant_df$income_main_source,
                                       levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                       labels = c("employment",
                                                  "unemployment_benefits",
                                                  "pension_or_rent",
                                                  "dependent_income",
                                                  "own_assets",
                                                  "social_assistance",
                                                  "hartz_iv",
                                                  "other_support",
                                                  "elterngeld"))

### Verify recoding
table(tenant_df$income_main_source, useNA = "ifany")

#####

##### 5.14) pension - EF402 #####

### Check for missing values and distribution
sum(is.na(tenant_df$pension))
table(tenant_df$pension, useNA = "ifany")

### Recode to binary dummy:
## 1 = pension → 1
## 8 = no_pension → 0
## others/NA → NA
tenant_df$pension <- case_when(
  tenant_df$pension == 1 ~ 1,
  tenant_df$pension == 8 ~ 0,
  TRUE ~ NA_real_
)

tenant_df$pension <- factor(tenant_df$pension, levels = c(0, 1), labels = c("no_pension", "pension"))

### Verify recoding
table(tenant_df$pension, useNA = "ifany")

#####

##### 5.15) income_assets - EF431 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$income_assets <- as.numeric(tenant_df$income_assets)

### Recode to binary dummy:
## 3 = assets_income → 1
## all others and NA → 0
tenant_df$income_assets <- case_when(
  tenant_df$income_assets == 3 ~ 1,
  TRUE ~ 0
)

tenant_df$income_assets <- factor(tenant_df$income_assets,
                                  levels = c(0, 1),
                                  labels = c("no_assets", "assets_income"))

### Verify recoding
table(tenant_df$income_assets, useNA = "ifany")

#####

##### 5.16) income_rent - EF433 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$income_rent <- as.numeric(tenant_df$income_rent)

### Recode to binary dummy:
## 5 = rent_income → 1
## all others and NA → 0
tenant_df$income_rent <- case_when(
  tenant_df$income_rent == 5 ~ 1,
  TRUE ~ 0
)

tenant_df$income_rent <- factor(tenant_df$income_rent,
                                levels = c(0, 1),
                                labels = c("no_rent", "rent_income"))

### Verify recoding
table(tenant_df$income_rent, useNA = "ifany")

#####

##### 5.17) building_type - EF489 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$building_type <- as.numeric(tenant_df$building_type)

### Remove missing values (including coded value 9 = "community accommodation")
tenant_df <- tenant_df %>%
  drop_na(building_type) %>%
  mutate(building_type = na_if(building_type, 9)) %>%
  drop_na(building_type)

### Recode to labeled factor:
tenant_df$building_type <- recode(tenant_df$building_type,
                                  `1` = "residential_building",
                                  `2` = "dormitory_with_household",
                                  `3` = "dormitory_without_household",
                                  `4` = "other_building_with_living_space")

tenant_df$building_type <- factor(tenant_df$building_type,
                                  levels = c("residential_building",
                                             "dormitory_with_household",
                                             "dormitory_without_household",
                                             "other_building_with_living_space"),
                                  labels = c("Residential Building",
                                             "Dormitory with Household",
                                             "Dormitory without Household",
                                             "Other Building with Living Space"))

### Verify recoding
table(tenant_df$building_type, useNA = "ifany")

#####

##### 5.18) living_space - EF492 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$living_space <- as.numeric(tenant_df$living_space)

### Check for missing values and distribution
sum(is.na(tenant_df$living_space))
table(tenant_df$living_space, useNA = "ifany")

#####

##### 5.19) rented_apartment - EF495 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$rented_apartment <- as.numeric(tenant_df$rented_apartment)

### Recode to binary dummy:
## 8 = rented → 1
## 1 = owned → 0
## others/NA → NA
tenant_df$rented_apartment <- ifelse(tenant_df$rented_apartment == 8, 1,
                                     ifelse(tenant_df$rented_apartment == 1, 0, NA))

### Verify recoding
sum(is.na(tenant_df$rented_apartment))
table(tenant_df$rented_apartment, useNA = "ifany")

#####

##### 5.20) rent_total - EF504 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$rent_total <- as.numeric(tenant_df$rent_total)

### Check for missing values and distribution
sum(is.na(tenant_df$rent_total))
summary(tenant_df$rent_total)

#####

##### 5.21) building_size - EF570 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$building_size <- as.numeric(tenant_df$building_size)

### Check for missing values and distribution
sum(is.na(tenant_df$building_size))
table(tenant_df$building_size)

### Recode to labeled factor:
tenant_df$building_size <- recode(tenant_df$building_size,
                                  `1` = "small_building",
                                  `2` = "medium_building",
                                  `3` = "large_building",
                                  `4` = "community_accommodation",
                                  `6` = "new_building",
                                  `7` = "new_building_current_year")

tenant_df$building_size <- factor(tenant_df$building_size,
                                  levels = c("small_building", "medium_building", "large_building", 
                                             "community_accommodation", "new_building", "new_building_current_year"),
                                  labels = c("Small Building", "Medium Building", "Large Building", 
                                             "Community Accommodation", "New Building", "New Building (Current Year)"))

### Verify recoding
table(tenant_df$building_size)

#####

##### 5.22) household_size - EF616 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$household_size <- as.numeric(tenant_df$household_size)

### Check for missing values and distribution
sum(is.na(tenant_df$household_size))
table(tenant_df$household_size)

#####

##### 5.23) rent_per_sqm - EF638 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$rent_per_sqm <- as.numeric(tenant_df$rent_per_sqm)

### Check for missing values and distribution
sum(is.na(tenant_df$rent_per_sqm))
table(tenant_df$rent_per_sqm, useNA = "ifany")

#####

##### 5.24) rent_burden - EF639 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$rent_burden <- as.numeric(as.character(tenant_df$rent_burden))

### Check for missing values and distribution
sum(is.na(tenant_df$rent_burden))
table(tenant_df$rent_burden, useNA = "ifany")

#####

##### 5.25) num_employed_hh - EF664 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$num_employed_hh <- as.numeric(tenant_df$num_employed_hh)

### Check for missing values and distribution
sum(is.na(tenant_df$num_employed_hh))
table(tenant_df$num_employed_hh, useNA = "ifany")

#####

##### 5.26) num_foreigners_hh - EF668 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$num_foreigners_hh <- as.numeric(tenant_df$num_foreigners_hh)

### Check for missing values and distribution
sum(is.na(tenant_df$num_foreigners_hh))
table(tenant_df$num_foreigners_hh, useNA = "ifany")

#####

##### 5.27) num_children_hh - EF669 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$num_children_hh <- as.numeric(tenant_df$num_children_hh)

### Check for missing values and distribution
sum(is.na(tenant_df$num_children_hh))
table(tenant_df$num_children_hh, useNA = "ifany")

#####

##### 5.28) hh_net_income - EF707 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$hh_net_income <- as.numeric(tenant_df$hh_net_income)

### Recode invalid values to NA:
## 50 = invalid, 99 = missing
tenant_df$hh_net_income <- ifelse(tenant_df$hh_net_income %in% c(50, 99),
                                  NA,
                                  tenant_df$hh_net_income)

### Check for missing values and distribution
sum(is.na(tenant_df$hh_net_income))
table(tenant_df$hh_net_income, useNA = "ifany")

#####

##### 5.29) hh_head_gender - EF731 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$hh_head_gender <- as.numeric(tenant_df$hh_head_gender)

### Recode to binary dummy:
## 1 = male → 1
## 2 = female → 0
## others/NA → NA
tenant_df$hh_head_gender <- ifelse(tenant_df$hh_head_gender == 1, 1,
                                   ifelse(tenant_df$hh_head_gender == 2, 0, NA))

### Check for missing values and distribution
sum(is.na(tenant_df$hh_head_gender))
table(tenant_df$hh_head_gender, useNA = "ifany")

#####

##### 5.30) hh_head_nationality - EF734 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$hh_head_nationality <- as.numeric(tenant_df$hh_head_nationality)

### Recode to binary dummy:
## 1 = German → 1
## 2 = Foreign → 0
## others/NA → NA
tenant_df$hh_head_nationality <- ifelse(tenant_df$hh_head_nationality == 1, 1,
                                        ifelse(tenant_df$hh_head_nationality == 2, 0, NA))

### Check for missing values and distribution
sum(is.na(tenant_df$hh_head_nationality))
table(tenant_df$hh_head_nationality, useNA = "ifany")

#####

##### 5.31) hh_head_marital_status - EF735 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$hh_head_marital_status <- as.numeric(tenant_df$hh_head_marital_status)

### Check for missing values and distribution
sum(is.na(tenant_df$hh_head_marital_status))
table(tenant_df$hh_head_marital_status)

### Recode to labeled factor:
## Civil union categories (6, 7, 8) merged
## 3 = separated_married (separate category)
tenant_df$hh_head_marital_status <- recode(tenant_df$hh_head_marital_status,
                                           `1` = "single",
                                           `2` = "married",
                                           `3` = "separated_married",
                                           `4` = "divorced",
                                           `5` = "widowed",
                                           `6` = "civil_union",
                                           `7` = "civil_union",
                                           `8` = "civil_union")

tenant_df$hh_head_marital_status <- factor(tenant_df$hh_head_marital_status,
                                           levels = c("single", "married", "separated_married", 
                                                      "divorced", "widowed", "civil_union"),
                                           labels = c("Single", "Married", "Separated (Married)", 
                                                      "Divorced", "Widowed", "Civil Union"))

### Verify recoding
table(tenant_df$hh_head_marital_status)

#####

##### 5.32) hh_head_employment - EF736 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$hh_head_employment <- as.numeric(tenant_df$hh_head_employment)

### Check for missing values and distribution
sum(is.na(tenant_df$hh_head_employment))
table(tenant_df$hh_head_employment)

### Recode to labeled factor:
tenant_df$hh_head_employment <- recode(tenant_df$hh_head_employment,
                                       `1` = "employed",
                                       `2` = "unemployed",
                                       `3` = "job_seeking",
                                       `4` = "other_inactive")

tenant_df$hh_head_employment <- factor(tenant_df$hh_head_employment,
                                       levels = c("employed", "unemployed", "job_seeking", "other_inactive"),
                                       labels = c("Employed", "Unemployed", "Job-seeking", "Other Inactive"))

### Verify recoding
table(tenant_df$hh_head_employment)

#####

##### 5.33) hh_head_income_source - EF741 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$hh_head_income_source <- as.numeric(tenant_df$hh_head_income_source)

### Check for missing values and distribution
sum(is.na(tenant_df$hh_head_income_source))
table(tenant_df$hh_head_income_source)

### Recode to labeled factor:
tenant_df$hh_head_income_source <- recode(tenant_df$hh_head_income_source,
                                          `1` = "employment",
                                          `2` = "unemployment_benefits",
                                          `3` = "pension_or_rent",
                                          `4` = "dependent_income",
                                          `5` = "own_assets",
                                          `6` = "social_assistance",
                                          `7` = "hartz_iv",
                                          `8` = "other_support",
                                          `9` = "elterngeld")

tenant_df$hh_head_income_source <- factor(tenant_df$hh_head_income_source,
                                          levels = c("employment",
                                                     "unemployment_benefits",
                                                     "pension_or_rent",
                                                     "dependent_income",
                                                     "own_assets",
                                                     "social_assistance",
                                                     "hartz_iv",
                                                     "other_support",
                                                     "elterngeld"))

### Verify recoding
table(tenant_df$hh_head_income_source)

#####

##### 5.34) hh_head_income - EF742 #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$hh_head_income <- as.numeric(tenant_df$hh_head_income)

### Recode invalid values (99) as NA
tenant_df$hh_head_income <- ifelse(tenant_df$hh_head_income == 99, NA, tenant_df$hh_head_income)

### Check for missing values and distribution
sum(is.na(tenant_df$hh_head_income))
table(tenant_df$hh_head_income, useNA = "ifany")

#####

##### 5.35) hh_head_education - EF743  #####

### Check for missing values and distribution
sum(is.na(tenant_df$hh_head_education))
table(tenant_df$hh_head_education)

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$hh_head_education <- as.numeric(tenant_df$hh_head_education)

### Recode values to grouped education levels
tenant_df$hh_head_education <- recode(tenant_df$hh_head_education,
                                      `1` = "hauptschule",           # Hauptschule
                                      `2` = "hauptschule",           # Polytechnische Oberschule DDR (8./9. Klasse)
                                      `3` = "realschule",            # Realschulabschluss
                                      `4` = "fachhochschulreife",    # Fachhochschulreife
                                      `5` = "abi",                   # Allgemeine/fachgebundene Hochschulreife
                                      `6` = "max_7_years_schooling", # Max. 7 Jahre Schule
                                      `7` = "no_info",               # Keine Angabe
                                      `8` = "no_degree",             # Kein Abschluss
                                      `9` = "realschule")            # POS DDR mit Abschluss 10. Klasse

### Convert to factor with descriptive labels
tenant_df$hh_head_education <- factor(tenant_df$hh_head_education,
                                      levels = c("hauptschule", "realschule", "fachhochschulreife", "abi", 
                                                 "max_7_years_schooling", "no_info", "no_degree"),
                                      labels = c("Primary School", 
                                                 "Secondary School", 
                                                 "Higher Education Entrance Qualification", 
                                                 "Abitur (General Higher Education Entrance Qualification)", 
                                                 "Max. 7 Years of Schooling", 
                                                 "No Information", 
                                                 "No Degree"))

### Verify recoding
table(tenant_df$hh_head_education)
#####

##### 5.36) hh_head_residence - EF746  #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$hh_head_residence <- as.numeric(tenant_df$hh_head_residence)

### Check for missing values and distribution
sum(is.na(tenant_df$hh_head_residence))
table(tenant_df$hh_head_residence)

### Recode to labeled factor
tenant_df$hh_head_residence <- recode(tenant_df$hh_head_residence,
                                      `1` = "main_residence_without_other",
                                      `2` = "main_residence_with_other",
                                      `3` = "secondary_residence_with_other"
)

tenant_df$hh_head_residence <- factor(tenant_df$hh_head_residence,
                                      levels = c("main_residence_without_other", 
                                                 "main_residence_with_other", 
                                                 "secondary_residence_with_other"),
                                      labels = c("Main Residence without Other", 
                                                 "Main Residence with Other", 
                                                 "Secondary Residence with Other")
)

### Verify recoding
table(tenant_df$hh_head_residence)

#####

##### 5.37) family_type - EF865  #####

### Convert to numeric (removes 'haven_labelled' class)
tenant_df$family_type <- as.numeric(tenant_df$family_type)

### Check for missing values and distribution
sum(is.na(tenant_df$family_type))
table(tenant_df$family_type)

### Recode to labeled factor
tenant_df$family_type <- recode(tenant_df$family_type,
                                `1` = "married_no_children",
                                `2` = "married_with_children",
                                `4` = "registered_partners_no_children",
                                `5` = "registered_partners_with_children",
                                `6` = "single_with_children",
                                `7` = "married_not_with_partner_no_children",
                                `8` = "married_not_with_partner_with_children",
                                `9` = "single_no_children"
)

tenant_df$family_type <- factor(tenant_df$family_type,
                                levels = c("married_no_children", "married_with_children", 
                                           "registered_partners_no_children", "registered_partners_with_children",
                                           "single_with_children", "married_not_with_partner_no_children",
                                           "married_not_with_partner_with_children", "single_no_children"),
                                labels = c("Married without Children", 
                                           "Married with Children", 
                                           "Registered Partnership without Children", 
                                           "Registered Partnership with Children",
                                           "Single with Children", 
                                           "Married (not living with partner), without children", 
                                           "Married (not living with partner), with children", 
                                           "Single without Children")
)

### Verify recoding
table(tenant_df$family_type)

#####

##### 6) Inspection of Variable Classes - tenant_df  #####

### Inspect variable classes
sapply(tenant_df, class)

#####

##### 7) Save dataset  #####

saveRDS(tenant_df, "data/tenant_df.rds")

#####


