library(DT)
library(ggplot2)
windows()

# Load dataset
df <- read.csv("exoplanets_filtered_default.csv", comment.char = "#", stringsAsFactors = FALSE)

# Get values for density and escape velocity
df$density_gcc <- 5.514 * (df$pl_bmasse / (df$pl_rade^3))  # density (g/cc)
df$v_esc_kms   <- 11.2  * sqrt(df$pl_bmasse / df$pl_rade)  # escape velocity (km/s)

# Keep needed rows
need <- c("pl_eqt","v_esc_kms","density_gcc")
df <- df[complete.cases(df[ , need]), ]

# Regression
df$can_hold_water <- ifelse(
  df$pl_eqt >= 200 & df$pl_eqt <= 350 &
  df$v_esc_kms >= 8 &
  df$density_gcc >= 3, 1, 0
)

model <- glm(can_hold_water ~ pl_eqt + v_esc_kms + density_gcc,
             data = df, family = binomial)

cat("\n==== Regression Summary ====\n")
print(summary(model))

# Set Regression results to weights
coef_vals   <- abs(coef(model)[-1])
reg_weights <- coef_vals / sum(coef_vals)

# Weights
w_T <- reg_weights["pl_eqt"]
w_V <- reg_weights["v_esc_kms"]
w_D <- reg_weights["density_gcc"]

cat("\n==== Derived Weights (from glm) ====\n")
print(reg_weights)
cat(sprintf("\nUsing weights -> Temperature: %.3f  |  Escape v: %.3f  |  Density: %.3f\n", w_T, w_V, w_D))

# Earth's data
earth_temp <- 288.15 # K
earth_vesc <- 11.19  # km/s
earth_dens <- 5.51   # g/cm^3

# Decay
k <- 0.8  # decay factor 
p <- 1.6  # contrast exponent 

# Temperature similarity
df$S_T <- (exp(-k * abs(df$pl_eqt    - earth_temp) / earth_temp))^p
# Escape-velocity similarity
df$S_V <- (exp(-k * abs(df$v_esc_kms - earth_vesc) / earth_vesc))^p
# Density similarity
df$S_D <- (exp(-k * abs(df$density_gcc - earth_dens) / earth_dens))^p

# Make sure weights are summed to 1
s <- w_T + w_V + w_D
w_T <- w_T / s;  w_V <- w_V / s;  w_D <- w_D / s

# Make WRS
df$WRS <- w_T * df$S_T + w_V * df$S_V + w_D * df$S_D

# Adding Earth to dataset
earth <- data.frame(
  pl_name     = "Earth",
  pl_eqt      = earth_temp,
  v_esc_kms   = earth_vesc,
  density_gcc = earth_dens,
  stringsAsFactors = FALSE
)

earth$S_T <- 1; earth$S_V <- 1; earth$S_D <- 1
earth$WRS <- w_T * earth$S_T + w_V * earth$S_V + w_D * earth$S_D  # == 1.0

# Add Earth
df_with_earth <- rbind(
  df[ , c("pl_name","pl_eqt","v_esc_kms","density_gcc","WRS")],
  earth[ , c("pl_name","pl_eqt","v_esc_kms","density_gcc","WRS")]
)

# Sort and view
df_ranked <- df_with_earth[order(-df_with_earth$WRS),
                           c("pl_name", "pl_eqt", "v_esc_kms", "density_gcc", "WRS")]

# Rounds to 2 decimal places
df_ranked$pl_eqt      <- round(df_ranked$pl_eqt, 2)
df_ranked$v_esc_kms   <- round(df_ranked$v_esc_kms, 2)
df_ranked$density_gcc <- round(df_ranked$density_gcc, 2)

# Rounds WRS to 4 decimal places
df_ranked$WRS <- round(df_ranked$WRS, 4)

# Export File
write.csv(df_ranked, "ranked_exoplanets_WRS.csv", row.names = FALSE)

# Interactive Table 
datatable(df_ranked,
          options = list(pageLength = 15, autoWidth = TRUE),
          caption = "Ranked Exoplanets by Earth-Centered Water Retention Score (WRS)") |>
  formatStyle(
    "pl_name",
    target = "row",
    backgroundColor = styleEqual(c("Earth"), c("#3ee784")),
    fontWeight      = styleEqual(c("Earth"), c("bold"))
  ) |>
  formatRound(c("pl_eqt", "v_esc_kms", "density_gcc", "WRS"), 3)

  # Histogram
ggplot(df_with_earth, aes(x = WRS)) +
  geom_histogram(bins = 30, fill = "deepskyblue", color = "white", alpha = 0.9) +
  geom_vline(xintercept = earth$WRS, color = "#96d863", linetype = "dashed", linewidth = 1) +
  annotate("text",
           x = earth$WRS + 0.02, y = Inf, vjust = 1.5,
           label = paste0("Earth (WRS = ", round(earth$WRS, 2), ")"),
           color = "red", size = 4.2) +
  labs(
    title = "Distribution of Water Retention Scores (WRS)",
    subtitle = "Dashed red line marks Earth's score",
    x = "Water Retention Score (0 = poor, 1 = ideal)",
    y = "Number of Exoplanets"
  ) +
  theme_minimal(base_size = 13)
