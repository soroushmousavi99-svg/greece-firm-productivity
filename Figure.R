# ============================================
# Script: 06_explore.R
# Explore and visualise Greece firm data
# ============================================

df_clean <- df_clean |>
  filter(!is.na(is_exporter))
nrow(df_clean)
table(df_clean$is_exporter)
nrow(df_clean)
table(df_clean$is_exporter)

library(tidyverse)
library(ggplot2)
# Figure 1 — productivity distribution by exporter status
ggplot(df_clean, aes(x = labour_prod, fill = factor(is_exporter))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#378ADD", "#1D9E75"),
                    labels = c("Non-exporter", "Exporter")) +
  labs(
    title = "Labour Productivity by Exporter Status",
    subtitle = "Greece, World Bank Enterprise Survey 2018-2023",
    x = "Log(Sales per Worker)",
    y = "Density",
    fill = NULL
  ) +
  theme_minimal()

ggsave("outputs/fig1_productivity_distribution.png", 
       width = 8, height = 5, dpi = 300)
