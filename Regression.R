nrow(df_clean)
# ============================================
# Script: 07_regressions.R
# Regression analysis — Greece firm data
# ============================================

library(tidyverse)
library(fixest)
library(modelsummary)

# Add log employees first
df_clean <- df_clean |>
  mutate(log_employees = log(l1))

# ---- Model 1: Raw gap ----
m1 <- feols(labour_prod ~ is_exporter, 
            data = df_clean)

summary(m1)

# ---- Model 2: Add controls ----
m2 <- feols(labour_prod ~ is_exporter + log_employees + 
              firm_age + I(firm_age^2), 
            data = df_clean)

summary(m2)

# ---- Model 3: Sector fixed effects ----
m3 <- feols(labour_prod ~ is_exporter + log_employees + 
              firm_age + I(firm_age^2) | sector, 
            data = df_clean)

summary(m3)

# ---- Model 4: Sector + Year fixed effects ----
m4 <- feols(labour_prod ~ is_exporter + log_employees + 
              firm_age + I(firm_age^2) | sector + year, 
            data = df_clean)

summary(m4)
library(modelsummary)
install.packages("webshot2")
modelsummary(
  list("Raw" = m1, 
       "Controls" = m2, 
       "Sector FE" = m3, 
       "Sector+Year FE" = m4),
  stars = TRUE,
  output = "outputs/table1_regression_results.png"
)
