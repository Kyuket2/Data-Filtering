
# Load datasets
df_full <- read.csv("csv/exoplanets_with_WRI_weighted.csv")
df_temp <- read.csv("csv/exoplanets_with_WRI_temp_only.csv")

idx <- match(df_full$pl_name, df_temp$pl_name)

df_compare <- data.frame(
  pl_name       = df_full$pl_name,
  WRI_weighted  = df_full$WRI_weighted,
  WRI_temp_only = df_temp$WRI_temp_only[idx]
)

# Difference column 
df_compare$WRI_difference <- df_compare$WRI_weighted - df_compare$WRI_temp_only

write.csv(df_compare[, c("pl_name","WRI_weighted","WRI_temp_only","WRI_difference")],
          "csv/WRI_model_comparison.csv",
          row.names = FALSE)

cat("Saved: csv/WRI_model_comparison.csv\n")

png("plots/WRI_scatter_comparison.png",
    width = 1000,
    height = 800)

plot(df_compare$WRI_temp_only,
     df_compare$WRI_weighted,
     pch = 19,
     col = rgb(0, 0, 1, 0.3),
     cex = 0.6,
     xlab = "Temperature-Only WRI",
     ylab = "Full Model WRI",
     main = "Comparison of Water Retention Index Scores")

grid()

abline(0,1,
       col = "red",
       lwd = 2,
       lty = 2)  

dev.off()
cat("Saved: plots/WRI_scatter_comparison.png\n")


png("plots/WRI_histograms.png",
    width = 1200,
    height = 600)

par(mfrow=c(1,2))

hist(df_compare$WRI_temp_only,
     col="skyblue",
     main="Temperature-Only WRI",
     xlab="WRI Score")

hist(df_compare$WRI_weighted,
     col="orange",
     main="Full Model WRI",
     xlab="WRI Score")

dev.off()
cat("Saved: plots/WRI_histograms.png\n")

correlation <- cor(df_compare$WRI_temp_only,
                   df_compare$WRI_weighted,
                   use = "complete.obs")

cat("Correlation between WRI scores:", correlation, "\n")


png("plots/WRI_difference_hist.png",
    width = 1000,
    height = 800)

hist(df_compare$WRI_difference,
     col="purple",
     main="Difference Between WRI Scores",
     xlab="Full WRI - Temp WRI")

dev.off()
cat("Saved: plots/WRI_difference_hist.png\n")