library(ggplot2)

# Load dataset
df <- read.csv("exoplanets_filtered_default.csv", comment.char = "#")

# Get values for density and escape velocity
df$density_gcc <- 5.514 * (df$pl_bmasse / (df$pl_rade^3))  # density
df$v_esc_kms <- 11.2 * sqrt(df$pl_bmasse / df$pl_rade)     # escape velocity

# Clean data
df_plot <- subset(df, !is.na(pl_eqt) & !is.na(v_esc_kms) & !is.na(density_gcc))
df_plot <- subset(df_plot, pl_eqt <= 800 & v_esc_kms <= 50 & density_gcc <= 20)

# Binary variable
df_plot$has_water <- ifelse(
  df_plot$pl_eqt >= 200 & df_plot$pl_eqt <= 350 &
  df_plot$v_esc_kms >= 8 &
  df_plot$density_gcc >= 3, 1, 0
)

# Regression model
model <- glm(has_water ~ pl_eqt + v_esc_kms + density_gcc,
             data = df_plot, family = binomial)

# Get absolute coefficients
coefs <- abs(coef(model)[-1])
importance <- data.frame(
  Feature = c("Equilibrium Temperature (K)", "Escape Velocity (km/s)", "Planetary Density (g/cm³)"),
  Importance = coefs / sum(coefs)
)

# Plot feature importance
ggplot(importance, aes(x = reorder(Feature, Importance), y = Importance, fill = Feature)) +
  geom_col(width = 0.6) +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(
    title = "Relative Importance of Factors in Water Retention",
    x = "Planetary Attribute",
    y = "Relative Weight (Normalized)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5))
