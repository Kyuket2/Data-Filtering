
install.packages("DT")
library(DT)
library(ggplot2)

# Load dataset
df <- read.csv("exoplanets_filtered_default.csv", comment.char = "#")

# Get values for density and escape velocity
df$density_gcc <- 5.514 * (df$pl_bmasse / (df$pl_rade^3))  # density
df$v_esc_kms <- 11.2 * sqrt(df$pl_bmasse / df$pl_rade)     # escape velocity

# Keep needed rows
need <- c("pl_eqt","v_esc_kms","density_gcc")
df <- df[complete.cases(df[ , need]), ]

# Temperature score - centered on 275 K, 200–350 range (clamped)
df$S_T <- 1 - abs(df$pl_eqt - 275) / 75
df$S_T <- pmax(pmin(df$S_T, 1), 0)

# Escape velocity score - ideal 8–20 km/s (clamped)
df$S_V <- (pmin(df$v_esc_kms, 20) - 8) / 12
df$S_V <- pmax(pmin(df$S_V, 1), 0)

# Density score - ideal 3–10 g/cm³ (clamped)
df$S_D <- (pmin(df$density_gcc, 10) - 3) / 7
df$S_D <- pmax(pmin(df$S_D, 1), 0)

# Weights
w_T <- 0.52
w_V <- 0.31
w_D <- 0.17

# WRS
df$WRS <- w_T * df$S_T + w_V * df$S_V + w_D * df$S_D

# Adding Earth as reference
earth <- data.frame(
  pl_name     = "Earth",
  pl_eqt      = 288,
  v_esc_kms   = 11.2,
  density_gcc = 5.51
)

earth$S_T <- 1 - abs(earth$pl_eqt - 275) / 75
earth$S_T <- pmax(pmin(earth$S_T, 1), 0)

earth$S_V <- (pmin(earth$v_esc_kms, 20) - 8) / 12
earth$S_V <- pmax(pmin(earth$S_V, 1), 0)

earth$S_D <- (pmin(earth$density_gcc, 10) - 3) / 7
earth$S_D <- pmax(pmin(earth$S_D, 1), 0)

earth$WRS <- w_T * earth$S_T + w_V * earth$S_V + w_D * earth$S_D

# Check to make sure Earth is within the dataset, then adding Earth
df$pl_name <- ifelse(is.null(df$pl_name), NA, df$pl_name)
df_with_earth <- rbind(
  df[ , c("pl_name","pl_eqt","v_esc_kms","density_gcc","WRS")],
  earth[ , c("pl_name","pl_eqt","v_esc_kms","density_gcc","WRS")]
)

# Sort and view
df_ranked <- df_with_earth[order(-df_with_earth$WRS), 
                           c("pl_name", "pl_eqt", "v_esc_kms", "density_gcc", "WRS")]

# Top and Bottom of WRS
head(df_ranked, 15)
tail(df_ranked, 10)

# Export File
write.csv(df_ranked, "ranked_exoplanets_WRS.csv", row.names = FALSE)

# Create Interactive Table with DT
datatable(df_ranked,
          options = list(pageLength = 15, autoWidth = TRUE),
          caption = "Ranked Exoplanets by Water Retention Score (WRS)")
