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

set.seed(0)

idx1 <- which(df$can_hold_water == 1)
idx0 <- which(df$can_hold_water == 0)

train_idx_70 <- c(
  sample(idx1, size = floor(0.7 * length(idx1))),
  sample(idx0, size = floor(0.7 * length(idx0)))
)

train70 <- df[train_idx_70, ]
test30  <- df[-train_idx_70, ]

table(train70$can_hold_water)

pos_train70 <- train70$pl_name[train70$can_hold_water == 1]
pos_test30  <- test30$pl_name[test30$can_hold_water == 1]

cat("\nPositives in TRAIN (70%):", length(pos_train70), "\n")
print(pos_train70)

cat("\nPositives in TEST (30%):", length(pos_test30), "\n")
print(pos_test30)

table(test30$can_hold_water)

model_firth_70 <- logistf(
  can_hold_water ~ pl_eqt + v_esc_kms + density_gcc,
  data = train70
)

coef_70 <- coef(model_firth_70)

weights_70 <- abs(coef_70[-1]) / sum(abs(coef_70[-1]))
weights_70

test30$prob <- predict(model_firth_70, newdata = test30, type = "response")
test30$predicted_water <- as.integer(test30$prob >= 0.5)

true_ones_test30 <- subset(test30, can_hold_water == 1)
recovered_70 <- subset(true_ones_test30, predicted_water == 1)

cat("Recovered (70% training):", nrow(recovered_70), "\n")
recovered_70$pl_name

top20_70 <- test30[order(test30$prob, decreasing = TRUE), ][1:20,
  c("pl_name", "prob", "can_hold_water")]

top20_70
cat("True water-capable in top 20:",
    sum(top20_70$can_hold_water == 1), "\n")

