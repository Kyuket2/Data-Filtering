library(logistf)

# Load dataset
df <- read.csv("csv/exoplanets_filtered_default.csv", comment.char = "#", stringsAsFactors = FALSE)

# Get values for density and escape velocity
df$density_gcc <- 5.514 * (df$pl_bmasse / (df$pl_rade^3))  # density (g/cc)
df$v_esc_kms   <- 11.2  * sqrt(df$pl_bmasse / df$pl_rade)  # escape velocity (km/s)

# Keep needed rows
need <- c("pl_eqt","v_esc_kms","density_gcc")
df <- df[complete.cases(df[ , need]), ]

# Regression label (ground truth)
df$can_hold_water <- ifelse(
  df$pl_eqt >= 200 & df$pl_eqt <= 350 &
    df$v_esc_kms >= 8 &
    df$density_gcc >= 3, 1, 0
)

run_seed <- function(seed, df, train_frac = 0.3, cutoff = 0.5, top_n = 20) {
  set.seed(seed)

  idx1 <- which(df$can_hold_water == 1)
  idx0 <- which(df$can_hold_water == 0)

  train_idx <- c(
    sample(idx1, size = floor(train_frac * length(idx1))),
    sample(idx0, size = floor(train_frac * length(idx0)))
  )

  train_df <- df[train_idx, ]
  test_df  <- df[-train_idx, ]

  model <- logistf::logistf(
    can_hold_water ~ pl_eqt + v_esc_kms + density_gcc,
    data = train_df
  )

  test_df$prob <- predict(model, newdata = test_df, type = "response")
  test_df$predicted_water <- as.integer(test_df$prob >= cutoff)

  true_ones_test <- subset(test_df, can_hold_water == 1)
  recovered <- subset(true_ones_test, predicted_water == 1)

  o <- order(test_df$prob, decreasing = TRUE)
  top_n2 <- min(top_n, nrow(test_df))
  top_tbl <- test_df[o, c("pl_name", "prob", "can_hold_water")][1:top_n2, ]

  pos_train <- train_df$pl_name[train_df$can_hold_water == 1]
  pos_test  <- test_df$pl_name[test_df$can_hold_water == 1]

  list(
    seed = seed,
    train_counts = table(train_df$can_hold_water),
    test_counts  = table(test_df$can_hold_water),

    positives_in_train = pos_train,
    positives_in_test  = pos_test,

    n_true_ones_test = nrow(true_ones_test),
    n_recovered = nrow(recovered),
    recovered_names = recovered$pl_name,

    top_tbl = top_tbl,
    n_true_in_top = sum(top_tbl$can_hold_water == 1)
  )
} 


# Set how many seeds will run
seeds <- 40:60
results <- lapply(seeds, run_seed, df = df, train_frac = 0.3, cutoff = 0.5, top_n = 20)


# Print out some results 
for (r in results) {
  cat("\n====================\n")
  cat("Seed:", r$seed, "\n")

  cat("Positives in TRAIN (", length(r$positives_in_train), "):\n", sep = "")
  if (length(r$positives_in_train) == 0) cat("(none)\n") else print(r$positives_in_train)

  cat("Positives in TEST (", length(r$positives_in_test), "):\n", sep = "")
  if (length(r$positives_in_test) == 0) cat("(none)\n") else print(r$positives_in_test)
}


# Print out the summary
for (r in results) {
  cat("\n====================\n")
  cat("Seed:", r$seed, "\n")
  cat("Train counts:\n"); print(r$train_counts)
  cat("Test counts:\n");  print(r$test_counts)
  cat("Original in test:", r$n_true_ones_test, "\n")
  cat("Recovered by model:", r$n_recovered, "\n")

  if (length(r$recovered_names) == 0) cat("Recovered planets: (none)\n")
  else { cat("Recovered planets:\n"); print(r$recovered_names) }

  cat("True water-capable in Top 20:", r$n_true_in_top, "\n")
}


# Add them to CSV's
summary_df <- data.frame(
  seed = sapply(results, `[[`, "seed"),
  recovered_count = sapply(results, `[[`, "n_recovered"),
  recovered_planets = sapply(results, function(x) paste(x$recovered_names, collapse = ", ")),
  true_in_top20 = sapply(results, `[[`, "n_true_in_top"),
  stringsAsFactors = FALSE
)

pos_split_df <- do.call(rbind, lapply(results, function(r) {
  data.frame(
    seed = r$seed,
    train_positives = paste(r$positives_in_train, collapse = ", "),
    test_positives  = paste(r$positives_in_test, collapse = ", "),
    stringsAsFactors = FALSE
  )
}))

write.csv(pos_split_df, "csv/positive_split_by_seed.csv", row.names = FALSE)
write.csv(summary_df, "csv/seed_recovery_summary.csv", row.names = FALSE)


# Recovered Frequency
all_recovered <- unlist(lapply(results, function(r) r$recovered_names))

if (length(all_recovered) == 0) {
  recovered_freq_df <- data.frame(pl_name = character(0), count = integer(0), percent_of_seeds = numeric(0))
} else {
  recovered_freq <- sort(table(all_recovered), decreasing = TRUE)
  recovered_freq_df <- data.frame(
    pl_name = names(recovered_freq),
    count = as.integer(recovered_freq),
    percent_of_seeds = 100 * as.integer(recovered_freq) / length(seeds),
    stringsAsFactors = FALSE
  )
}

write.csv(recovered_freq_df, "csv/recovered_planet_frequency.csv", row.names = FALSE)


# Top 20 frequency
all_top20 <- unlist(lapply(results, function(r) as.character(r$top_tbl$pl_name)))
top20_freq <- sort(table(all_top20), decreasing = TRUE)

top20_freq_df <- data.frame(
  pl_name = names(top20_freq),
  count = as.integer(top20_freq),
  percent_of_seeds = 100 * as.integer(top20_freq) / length(seeds),
  stringsAsFactors = FALSE
)

write.csv(top20_freq_df, "csv/top20_planet_frequency.csv", row.names = FALSE)
