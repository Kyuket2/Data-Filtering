library(DT)
library(ggplot2)
windows()

# Load dataset
df <- read.csv("exoplanets_filtered_default.csv", comment.char = "#", stringsAsFactors = FALSE)

# Get values for density and escape velocity
df$density_gcc <- 5.514 * (df$pl_bmasse / (df$pl_rade^3))  # density (g/cc)
df$v_esc_kms   <- 11.2  * sqrt(df$pl_bmasse / df$pl_rade)  # escape velocity (km/s)

# Regression
df$can_hold_water <- ifelse(
  df$pl_eqt >= 200 & df$pl_eqt <= 350 &
  df$v_esc_kms >= 8 &
  df$density_gcc >= 3, 1, 0
)

# Keep needed rows
need <- c("pl_eqt","v_esc_kms","density_gcc")
df <- df[complete.cases(df[ , need]), ]

# Full data
model_full  <- glm(can_hold_water ~ pl_eqt + v_esc_kms + density_gcc,
                   data = df, family = binomial)

# Set the seed
set.seed(42)

# Clean names
df$pl_name <- trimws(df$pl_name)

# Make an Earth row
if (!"Earth" %in% df$pl_name) {
  earth_row <- as.data.frame(setNames(as.list(rep(NA, ncol(df))), names(df)))

  # Fill the fields
  earth_row$pl_name        <- "Earth"
  earth_row$pl_eqt         <- 288
  earth_row$v_esc_kms      <- 11.2
  earth_row$density_gcc    <- 5.51
  earth_row$can_hold_water <- 1

  # Bind it
  df <- rbind(df, earth_row)
}
# Choose sample size
n_small <- min(500, nrow(df))

# Stratified sample
pos_idx   <- which(df$can_hold_water == 1 & non_earth)
neg_idx   <- which(df$can_hold_water == 0 & non_earth)

# Target split
target_pos <- max(1, min(length(pos_idx), ceiling(n_small * 0.30)))
target_neg <- max(1, min(length(neg_idx), n_small - target_pos - 1))  # -1 reserves Earth

# Sample indices
pos_take <- if (length(pos_idx) > 0) sample(pos_idx, target_pos) else integer(0)
neg_take <- if (length(neg_idx) > 0) sample(neg_idx, target_neg) else integer(0)

# Build small sample and append Earth
df_small <- rbind(
  df[pos_take, , drop = FALSE],
  df[neg_take, , drop = FALSE],
  df[df$pl_name == "Earth", , drop = FALSE]
)

# Trim if overshot, but keep Earth
if (nrow(df_small) > n_small) {
  earth_i  <- which(df_small$pl_name == "Earth")[1]
  keep_idx <- c(
    earth_i,
    setdiff(seq_len(nrow(df_small)), earth_i)[1:(n_small - 1)]
  )
  df_small <- df_small[keep_idx, , drop = FALSE]
}

#  Make sure earth is preset
stopifnot("Earth" %in% df_small$pl_name)

# Look at the small sample
View(df_small)

# Fit to small model
model_small <- glm(can_hold_water ~ pl_eqt + v_esc_kms + density_gcc,
                   data = df_small, family = binomial)

cat("\n=== Full Model: ===\n");   print(summary(model_full))
cat("\n=== Small Model: ===\n");  print(summary(model_small))

# Normalized weights
coef_full  <- abs(coef(model_full )[-1]); w_full  <- coef_full  / sum(coef_full)
coef_small <- abs(coef(model_small)[-1]); w_small <- coef_small / sum(coef_small)

cat("\nWeights (full):\n");  print(round(w_full , 3))
cat("\nWeights (small):\n"); print(round(w_small, 3))

# Bar chart of weights
weights_df <- data.frame(
  factor = rep(names(w_full), 2),
  weight = c(as.numeric(w_full), as.numeric(w_small)),
  model  = rep(c("Full data","Small sample"), each = length(w_full))
)

ggplot(weights_df, aes(x = factor, y = weight, fill = model)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = sprintf("%.2f", weight)),
            position = position_dodge(0.9), vjust = -0.2, size = 3) +
  labs(title = "Logistic-regression–derived weights: Full vs Small sample",
       x = "Factor", y = "Normalized weight (sum = 1)") +
  theme_minimal(base_size = 13)

# WRS / Earth defaults
earth_temp <- 288.15; earth_vesc <- 11.19; earth_dens <- 5.51
k <- 0.8; p <- 1.6

S_T <- (exp(-k * abs(df$pl_eqt    - earth_temp) / earth_temp))^p
S_V <- (exp(-k * abs(df$v_esc_kms - earth_vesc)  / earth_vesc ))^p
S_D <- (exp(-k * abs(df$density_gcc - earth_dens) / earth_dens))^p

# Helpers
wf <- function(nm) as.numeric(w_full [nm])
ws <- function(nm) as.numeric(w_small[nm])

WRS_full  <- wf("pl_eqt")*S_T + wf("v_esc_kms")*S_V + wf("density_gcc")*S_D
WRS_small <- ws("pl_eqt")*S_T + ws("v_esc_kms")*S_V + ws("density_gcc")*S_D

# Agreement Metrics
cat("\n=== Agreement between WRS (Full vs Small) ===\n")
cat("Spearman rho : ", round(cor(WRS_full, WRS_small, method = "spearman"), 3), "\n")
cat("Kendall  tau : ", round(cor(WRS_full, WRS_small, method = "kendall"),  3), "\n")

# Top-N overlap
N <- 50
idx_full  <- order(WRS_full , decreasing = TRUE)[1:N]
idx_small <- order(WRS_small, decreasing = TRUE)[1:N]
overlap <- length(intersect(idx_full, idx_small)) / N
cat(sprintf("Top-%d overlap: %.1f%%\n", N, 100 * overlap))

# Biggest change
df_compare <- data.frame(
  pl_name   = df$pl_name,
  WRS_full  = round(WRS_full,  3),
  WRS_small = round(WRS_small, 3)
)
df_compare$delta <- df_compare$WRS_small - df_compare$WRS_full