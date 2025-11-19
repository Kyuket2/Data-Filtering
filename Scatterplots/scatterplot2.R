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
ggplot(df_plot, aes(x = pl_eqt, y = density_gcc, color = v_esc_kms)) +
  annotate("rect",
           xmin = 200, xmax = 350,
           ymin = 3, ymax = 10,
           alpha = 0.1, fill = "skyblue", color = "deepskyblue") +
  annotate("text",
           x = 275, y = 10.8,
           label = "Liquid Water Zone",
           color = "deepskyblue", size = 4, fontface = "bold") +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_viridis_c(option = "plasma", direction = -1) +
  labs(
    title = "Temperature vs Planetary Density",
    subtitle = "Color represents Escape Velocity (km/s)",
    x = "Equilibrium Temperature (K)",
    y = "Planetary Density (g/cm³)",
    color = "Escape Velocity (km/s)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
