
# Load datasets
df_full <- read.csv("csv/exoplanets_with_WRI_weighted.csv")
df_temp <- read.csv("csv/exoplanets_with_WRI_temp_only.csv")

# Merge them by the exoplanet name
df_compare <- merge(df_full, df_temp,
                    by = "pl_name",
                    suffixes = c("_full", "_temp"))


# Scatter Plot Comparison
png("plots/WRI_scatter_comparison.png",
    width = 1000,
    height = 800)

# Color density
point_density <- densCols(df_compare$WRI_temp_only,
                          df_compare$WRI_weighted)

plot(df_compare$WRI_temp_only,
     df_compare$WRI_weighted,
     col = point_density,
     pch = 16,
     cex = 0.7,
     xlab = "Temperature-Only Water Retention Index",
     ylab = "Full Model Water Retention Index",
     main = "Comparison of Water Retention Index Models")

# Add grid
grid()

# Agreement line
abline(0,1,
       col="red",
       lwd=3,
       lty=2)

dev.off()

# Histogram Comparison of the two
par(mfrow=c(1,2))

hist(df_compare$WRI_temp_only,
     col="skyblue",
     main="Temperature-Only WRI",
     xlab="WRI Score")

hist(df_compare$WRI_weighted,
     col="orange",
     main="Full Model WRI",
     xlab="WRI Score")

par(mfrow=c(1,1))


# Correlation Between Scores
correlation <- cor(df_compare$WRI_temp_only,
                   df_compare$WRI_weighted)

cat("Correlation between scores:", correlation, "\n")


# Difference Analysis
df_compare$WRI_difference <- df_compare$WRI_weighted -
                            df_compare$WRI_temp_only

summary(df_compare$WRI_difference)
