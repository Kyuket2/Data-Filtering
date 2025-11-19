library(DT)
library(ggplot2)
windows()

# Load dataset
df <- read.csv("csv/exoplanets_filtered_default.csv", comment.char = "#", stringsAsFactors = FALSE)

# Get values for density and escape velocity
df$density_gcc <- 5.514 * (df$pl_bmasse / (df$pl_rade^3))  # density (g/cc)
df$v_esc_kms   <- 11.2  * sqrt(df$pl_bmasse / df$pl_rade)  # escape velocity (km/s)

# Keep needed rows
need <- c("pl_eqt","v_esc_kms","density_gcc")
df <- df[complete.cases(df[ , need]), ]

# Add in Earth
df[nrow(df) + 1, c("pl_eqt","v_esc_kms","density_gcc")] <- c(288.15, 11.2, 5.51)

# Regression
df$can_hold_water <- ifelse(
  df$pl_eqt >= 200 & df$pl_eqt <= 350 &
  df$v_esc_kms >= 8 &
  df$density_gcc >= 3, 1, 0
)

# Model
model <- glm(can_hold_water ~ pl_eqt + v_esc_kms + density_gcc,
             data = df, family = binomial)

# Check to see how many rare events are present.
table(df$can_hold_water)
prop.table(table(df$can_hold_water))

count_df <- data.frame(table(df$can_hold_water))
names(count_df) <- c("class", "count")

ggplot(count_df, aes(x = class, y = count, fill = class)) +
  geom_col() +
  geom_text(aes(label = count), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("gray70", "steelblue"),
                    labels = c("0" = "Non-event", "1" = "Rare event")) +
  scale_x_discrete(labels = c("0" = "Non-event (0)", "1" = "Rare event (1)")) +
  labs(
    title = "Rare Events in Dataset",
    x = "Class",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

