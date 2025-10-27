
library(dplyr)
library(DT)
library(ggplot2)

# Load
df <- read.csv("exoplanets_filtered_default.csv",
               comment.char = "#", stringsAsFactors = FALSE)

cat("At load — unique pl_name:", length(unique(df$pl_name)), "\n")
print(head(unique(df$pl_name), 8))

# Physics
df <- df %>%
  mutate(
    density_gcc = 5.514 * (pl_bmasse / (pl_rade^3)),
    v_esc_kms   = 11.2  * sqrt(pl_bmasse / pl_rade)
  )

# Keep  certain rows
need <- c("pl_eqt","v_esc_kms","density_gcc")
df <- df[complete.cases(df[ , need]), ]

cat("After filter — unique pl_name:", length(unique(df$pl_name)), "\n")
print(head(df[ , c("pl_name","hostname")], 10))

# Scores (clamped)
S_T <- 1 - abs(df$pl_eqt - 275) / 75
S_V <- (pmin(df$v_esc_kms, 20) - 8) / 12
S_D <- (pmin(df$density_gcc, 10) - 3) / 7
df$S_T <- pmax(pmin(S_T, 1), 0)
df$S_V <- pmax(pmin(S_V, 1), 0)
df$S_D <- pmax(pmin(S_D, 1), 0)

# WRS
w_T <- 0.52; w_V <- 0.31; w_D <- 0.17
df$WRS <- w_T*df$S_T + w_V*df$S_V + w_D*df$S_D

# Earth Data
earth <- data.frame(
  pl_name     = "Earth",
  pl_eqt      = 288,
  v_esc_kms   = 11.2,
  density_gcc = 5.51,
  stringsAsFactors = FALSE
)
earth$S_T <- pmax(pmin(1 - abs(earth$pl_eqt - 275)/75, 1), 0)
earth$S_V <- pmax(pmin((pmin(earth$v_esc_kms, 20) - 8)/12, 1), 0)
earth$S_D <- pmax(pmin((pmin(earth$density_gcc, 10) - 3)/7, 1), 0)
earth$WRS <- w_T*earth$S_T + w_V*earth$S_V + w_D*earth$S_D

view_cols <- c("pl_name","pl_eqt","v_esc_kms","density_gcc","WRS")
df_with_earth <- rbind(df[ , view_cols], earth[ , view_cols])

cat("Final — unique pl_name:", length(unique(df_with_earth$pl_name)), "\n")
print(head(df_with_earth, 10))

# Data from Table
df_ranked <- df_with_earth[order(-df_with_earth$WRS), view_cols]
write.csv(df_ranked, "ranked_exoplanets_WRS.csv", row.names = FALSE)

datatable(df_ranked,
          options = list(pageLength = 15, autoWidth = TRUE),
          caption = "Ranked Exoplanets by Water Retention Score (WRS)")
