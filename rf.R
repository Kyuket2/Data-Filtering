# install.packages("randomForest")

library(randomForest)

# Load dataset
df <- read.csv("csv/exoplanets_filtered_default.csv", comment.char = "#", stringsAsFactors = FALSE)

# Get values for density and escape velocity
df$density_gcc <- 5.514 * (df$pl_bmasse / (df$pl_rade^3))  # density (g/cc)
df$v_esc_kms   <- 11.2  * sqrt(df$pl_bmasse / df$pl_rade)  # escape velocity (km/s)

# Keep needed rows
need <- c("pl_eqt","v_esc_kms","density_gcc")
df <- df[complete.cases(df[ , need]), ]

df$HT_class <- with(df,
  ifelse(pl_eqt >= 273.15 & pl_eqt <= 323.15, "complex",
  ifelse(pl_eqt >= 253.15 & pl_eqt <= 395.15, "microbial",
         "limited"))
)

df$HT_class <- as.factor(df$HT_class)


set.seed(42)

rf_model <- randomForest(
  HT_class ~ pl_eqt + v_esc_kms + density_gcc,
  data = df,
  ntree = 500,
  importance = TRUE
)

print(rf_model)
importance(rf_model)

# This is to test without temp: 
# ____________________________________________________________________________________________

# Load dataset
df <- read.csv("csv/exoplanets_filtered_default.csv", comment.char = "#", stringsAsFactors = FALSE)

# Get values for density and escape velocity
df$density_gcc <- 5.514 * (df$pl_bmasse / (df$pl_rade^3))  # density (g/cc)
df$v_esc_kms   <- 11.2  * sqrt(df$pl_bmasse / df$pl_rade)  # escape velocity (km/s)

# Keep needed rows
need <- c("pl_eqt","v_esc_kms","density_gcc")
df <- df[complete.cases(df[ , need]), ]

df$HT_class <- with(df,
  ifelse(pl_eqt >= 273.15 & pl_eqt <= 323.15, "complex",
  ifelse(pl_eqt >= 253.15 & pl_eqt <= 395.15, "microbial",
         "limited"))
)

set.seed(42)

df$HT_class <- as.factor(df$HT_class)


rf_no_temp <- randomForest(
  HT_class ~ v_esc_kms + density_gcc,
  data = df,
  ntree = 500,
  importance = TRUE
)

print(rf_no_temp)
importance(rf_no_temp)


table(df$HT_class)
