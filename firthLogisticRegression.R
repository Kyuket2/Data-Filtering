library(logistf)

# Load dataset
df <- read.csv("csv/exoplanets_filtered_default.csv",
               comment.char = "#",
               stringsAsFactors = FALSE)

# Get values for density and escape velocity
df$density_gcc <- 5.514 * (df$pl_bmasse / (df$pl_rade^3))  # density (g/cc)
df$v_esc_kms   <- 11.2  * sqrt(df$pl_bmasse / df$pl_rade)  # escape velocity (km/s)

# Keep needed rows
need <- c("pl_eqt","v_esc_kms","density_gcc")
df <- df[complete.cases(df[, need]), ]

# Logic for regression
df$can_hold_water <- ifelse(
  df$pl_eqt >= 200 & df$pl_eqt <= 350 &
  df$v_esc_kms >= 8 &
  df$density_gcc >= 3, 1, 0
)

# Firth logistic regression 
model_firth <- logistf(can_hold_water ~ pl_eqt + v_esc_kms + density_gcc,
                       data = df)

# Get coefficients
coef_firth   <- coef(model_firth)
firth_vals   <- abs(coef_firth[-1])   # removes the intercept
firth_weights <- firth_vals / sum(firth_vals)

# Show weights
print(firth_weights)

# Standardize predictors
X <- scale(df[, c("pl_eqt", "v_esc_kms", "density_gcc")])

# WRI
wri_raw <- as.numeric(X %*% firth_weights)

# Normalize to 0–1
df$WRI_weighted <- (wri_raw - min(wri_raw)) /
                   (max(wri_raw) - min(wri_raw))

# Save to a CSV
write.csv(df,
          "csv/exoplanets_with_WRI_weighted.csv",
          row.names = FALSE)

head(df$WRI_weighted)