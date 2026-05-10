# ============================================
# Script: 08_lasso.R
# LASSO propensity scores
# ============================================

library(tidyverse)
library(glmnet)
library(fixest)
library(modelsummary)

# ============================================
# Step 1 — Build feature matrix
# ============================================

# Select firm characteristics that predict exporter status
X <- df_clean |>
  select(log_employees, firm_age, is_foreign, b2a) |>
  as.matrix()

# Our outcome — exporter status
y <- df_clean$is_exporter

# Check dimensions
dim(X)
length(y)
head(X)
# ============================================
# Step 2 — Run LASSO with cross-validation
# ============================================

library(glmnet)

# ---- Remove NAs and rebuild clean dataset ----
df_lasso <- df_clean |>
  drop_na(log_employees, firm_age, is_foreign, b2a, is_exporter)

# Check how many rows remain
nrow(df_lasso)

# ---- Build feature matrix ----
X <- df_lasso |>
  select(log_employees, firm_age, is_foreign, b2a) |>
  as.matrix()

# Build outcome vector
y <- df_lasso$is_exporter

# Check dimensions
dim(X)
length(y)

# ---- Run LASSO ----
set.seed(42)
cv_lasso <- cv.glmnet(X, y, family = "binomial", alpha = 1)

# Best lambda
best_lambda <- cv_lasso$lambda.min
cat("Best lambda:", best_lambda)

# Plot cross-validation results
plot(cv_lasso)

nrow(df_lasso)
  best_lambda
  
# Extract coefficients at best lambda
coef(cv_lasso, s = "lambda.min")  

# Extract propensity scores for all firms
df_lasso$prop_score <- predict(
  cv_lasso, 
  newx = X, 
  s = "lambda.min", 
  type = "response"
) |> as.vector()

# Check distribution
summary(df_lasso$prop_score)

# Plot propensity scores by exporter status
ggplot(df_lasso, aes(x = prop_score, 
                     fill = factor(is_exporter))) +
  geom_histogram(bins = 40, alpha = 0.6, 
                 position = "identity") +
  scale_fill_manual(values = c("#378ADD", "#1D9E75"),
                    labels = c("Non-exporter", "Exporter")) +
  labs(
    title = "LASSO Propensity Scores by Exporter Status",
    x = "P(Exporter | Firm Characteristics)",
    y = "Number of firms",
    fill = NULL
  ) +
  theme_minimal()

# ---- Step 4: Inverse Probability Weights ----
df_lasso <- df_lasso |>
  mutate(
    ipw_weight = case_when(
      is_exporter == 1 ~ 1 / prop_score,
      is_exporter == 0 ~ 1 / (1 - prop_score)
    ),
    # Trim extreme weights at 99th percentile
    ipw_weight = pmin(ipw_weight, 
                      quantile(ipw_weight, 0.99))
  )

# Check weights
summary(df_lasso$ipw_weight)

# Convert haven labelled variables to regular factors
library(haven)

df_lasso <- df_lasso |>
  mutate(
    sector = as_factor(sector),
    year   = as_factor(year)
  )

# Run weighted regression
m_ipw <- feols(
  labour_prod ~ is_exporter + log_employees + 
    firm_age + I(firm_age^2) | sector + year,
  weights = ~ipw_weight,
  data = df_lasso
)

summary(m_ipw)

# Need to rerun m4 on df_lasso for fair comparison
m4_lasso <- feols(
  labour_prod ~ is_exporter + log_employees + 
    firm_age + I(firm_age^2) | sector + year,
  data = df_lasso
)

# Final comparison table
modelsummary(
  list("FE Unweighted" = m4_lasso,
       "LASSO-IPW Weighted" = m_ipw),
  stars = TRUE,
  output = "outputs/table2_ipw_comparison.png"
)

# Save propensity score plot
ggplot(df_lasso, aes(x = prop_score, 
                     fill = factor(is_exporter))) +
  geom_histogram(bins = 40, alpha = 0.6, 
                 position = "identity") +
  scale_fill_manual(values = c("#378ADD", "#1D9E75"),
                    labels = c("Non-exporter", "Exporter")) +
  labs(
    title = "LASSO Propensity Scores by Exporter Status",
    x = "P(Exporter | Firm Characteristics)",
    y = "Number of firms",
    fill = NULL
  ) +
  theme_minimal()

# Save it
ggsave("outputs/fig2_propensity_scores.png", 
       width = 8, height = 5, dpi = 300)