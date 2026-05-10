# ============================================
# Greece Firm Productivity Project
# Script: 05_clean_final.R
# Author: Soroush
# Date: May 2026
# ============================================

# ---- Load packages ----
library(haven)
library(tidyverse)

# ---- Load data ----
df <- read_dta("Greece_2018_2023.dta")

# ---- First look ----
nrow(df)
ncol(df)

# ---- Select key variables ----
df_clean <- df |>
  select(panelid, year, e1, l1, d2, b2a, b5, sector, region, size)

# ---- Check missing values ----
colSums(is.na(df_clean))

# ---- Check impossible values ----
summary(df_clean$l1)
summary(df_clean$d2)
summary(df_clean$e1)
summary(df_clean$b2a)
summary(df_clean$b5)

# ---- Clean negative values ----
df_clean <- df_clean |>
  mutate(
    e1  = ifelse(e1  < 0, NA, e1),
    b2a = ifelse(b2a < 0, NA, b2a),
    l1  = ifelse(l1  < 0, NA, l1),
    d2  = ifelse(d2  < 0, NA, d2)
  )

# ---- Create exporter dummy ----
df_clean <- df_clean |>
  mutate(is_exporter = ifelse(e1 == 1, 0, 1))

# ---- Verify ----
nrow(df_clean)
ncol(df_clean)
table(df_clean$is_exporter)
df_clean <- df_clean |>
  mutate(labour_prod = log(d2 / l1))
summary(df_clean$labour_prod)
df_clean <- df_clean |>
  mutate(
    firm_age   = year - b5,
    is_foreign = ifelse(b2a < 50, 1, 0)
  )

summary(df_clean$firm_age)
table(df_clean$is_foreign)

attr(df$b2a, "label")
attr(df$e1, "label")
attr(df$l1, "label")
attr(df$d2, "label")
attr(df$b5, "label")
table(df_clean$b2a)