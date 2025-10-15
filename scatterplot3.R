library(ggplot2)

# Load dataset
df <- read.csv("exoplanets_filtered_default.csv", comment.char = "#")

# Get values for density and escape velocity
df$density_gcc <- 5.514 * (df$pl_bmasse / (df$pl_rade^3))  # density
df$v_esc_kms <- 11.2 * sqrt(df$pl_bmasse / df$pl_rade)     # escape velocity

# Clean data
df_plot <- subset(df, !is.na(pl_eqt) & !is.na(v_esc_kms) & !is.na(density_gcc))
df_plot <- subset(df_plot, pl_eqt <= 800 & v_esc_kms <= 50 & density_gcc <= 20)

# Scatterplot
ggplot(df_plot, aes(x = v_esc_kms, y = density_gcc, color = pl_eqt)) +
  annotate("rect",
           xmin = 8, xmax = 20,
           ymin = 3, ymax = 10,
           alpha = 0.1, fill = "skyblue", color = "deepskyblue") +
  annotate("text",
           x = 10, y = 10.8,
           label = "Liquid Water Zone",
           color = "deepskyblue", size = 4, fontface = "bold") +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_viridis_c(option = "inferno", direction = -1) +
  labs(
    title = "Escape Velocity vs Planetary Density",
    subtitle = "Color represents Equilibrium Temperature (K)",
    x = "Escape Velocity (km/s)",
    y = "Planetary Density (g/cm³)",
    color = "Equilibrium Temperature (K)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
