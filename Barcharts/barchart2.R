# Packages
library(ggplot2)
library(dplyr)

# Load dataset
df <- read.csv("exoplanets_filtered_default.csv", comment.char = "#")

# Get values for density and escape velocity
df$density_gcc <- 5.514 * (df$pl_bmasse / (df$pl_rade^3))  # density
df$v_esc_kms <- 11.2 * sqrt(df$pl_bmasse / df$pl_rade)     # escape velocity

# Keep only complete rows
dfm <- subset(df, !is.na(pl_eqt) & !is.na(v_esc_kms) & !is.na(density_gcc))

# Binary variable
dfm$has_water <- ifelse(
  dfm$pl_eqt >= 200 & dfm$pl_eqt <= 350 &
  dfm$v_esc_kms >= 8 &
  dfm$density_gcc >= 3, 1, 0
)

#  Logistic regression (unscaled)
mod_raw <- glm(has_water ~ pl_eqt + v_esc_kms + density_gcc,
               data = dfm, family = binomial)

raw_coefs <- abs(coef(mod_raw)[-1])
raw_importance <- raw_coefs / sum(raw_coefs)

# Logistic regression (standardized)
  mutate(across(c(pl_eqt, v_esc_kms, density_gcc), scale,
                .names = "{col}_z"))

mod_scaled <- glm(has_water ~ pl_eqt_z + v_esc_kms_z + density_gcc_z,
                  data = dfm, family = binomial)

scaled_coefs <- abs(coef(mod_scaled)[-1])
scaled_importance <- scaled_coefs / sum(scaled_coefs)

# Combine
nice_names <- c("pl_eqt"="Equilibrium Temperature (K)",
                "v_esc_kms"="Escape Velocity (km/s)",
                "density_gcc"="Planetary Density (g/cm³)")

imp_compare <- data.frame(
  Feature = rep(nice_names, 2),
  Importance = c(raw_importance, scaled_importance),
  Model = rep(c("Unscaled", "Standardized"), each=3)
)

# Plot comparison
ggplot(imp_compare, aes(x = Feature, y = Importance,
                        fill = Model)) +
  geom_col(position="dodge", width=0.6) +
  geom_text(aes(label = paste0(round(Importance*100,1), "%")),
            position = position_dodge(width=0.6),
            vjust = -0.4, size = 4) +
  scale_fill_viridis_d() +
  labs(
    title = "Effect of Variable Scaling on Logistic Regression Importance",
    subtitle = "Unscaled vs standardized predictors (absolute coefficients normalized)",
    x = "Planetary Attribute",
    y = "Relative Importance"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face="bold", hjust=0.5))
