install.packages("randomForest")
library(randomForest)

# Load the CSV
df2 <- read.csv("csv/exoplanets_with_WRI_weighted.csv")

# Convert it to a factor for the model
df2$can_hold_water <- as.factor(df2$can_hold_water)

# Train the model
set.seed(42)

rf_model <- randomForest(
  can_hold_water ~ pl_eqt + v_esc_kms + density_gcc + WRI_weighted,
  data = df2,
  ntree = 500,
  importance = TRUE
)

# Print out the results
print(rf_model)
importance(rf_model)

# End
