library(logistf)

# Load dataset
df <- read.csv("csv/exoplanets_filtered_default.csv", comment.char = "#", stringsAsFactors = FALSE)

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

# Models
model_firth <- logistf(can_hold_water ~ pl_eqt + v_esc_kms + density_gcc,
                       data = df)

# Coefficients
coef_firth <- coef(model_firth)

# Weights derived from coefficients
firth_vals   <- abs(coef_firth[-1])
firth_weights <- firth_vals / sum(firth_vals)

# Count all to which have a WRS = 1 (Including Earth)
sum(df$can_hold_water == 1)

# Selecting all to have a WRS = 1
subset(df, can_hold_water == 1, select = pl_name)