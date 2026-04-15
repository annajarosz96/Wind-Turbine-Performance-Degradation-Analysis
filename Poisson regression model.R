# Shift active_power_kw to be non-negative by adding the absolute value of the minimum if negative
min_val <- min(df$active_power_kw, na.rm = TRUE)
if (min_val < 0) {
  df$active_power_kw_shifted <- round(df$active_power_kw + abs(min_val))
} else {
  df$active_power_kw_shifted <- round(df$active_power_kw)
}

# Fit Poisson regression on shifted and rounded counts
poisson_model <- glm(active_power_kw_shifted ~ wind_speed_ms, data = df, family = poisson())

# Plot with shifted values
plot_poisson <- ggplot(df, aes(x = wind_speed_ms, y = active_power_kw_shifted)) +
  geom_point(color = "blue") +
  stat_smooth(method = "glm", method.args = list(family = poisson()), se = TRUE, color = "black") +
  labs(title = "Poisson Regression Model",
       x = "Wind Speed (m/s)",
       y = " Active Power (kW)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(plot_poisson)

# Summary statistics on shifted data
logLik_value <- as.numeric(logLik(poisson_model))
k <- length(coef(poisson_model))
n <- nrow(df)
AIC_value <- 2 * k - 2 * logLik_value
BIC_value <- k * log(n) - 2 * logLik_value
WAIC_value <- -2 * logLik_value

quantiles <- quantile(df$active_power_kw_shifted, probs = c(0.01, 0.03, 0.05), na.rm = TRUE)
min_val_shifted <- min(df$active_power_kw_shifted, na.rm = TRUE)
max_val_shifted <- max(df$active_power_kw_shifted, na.rm = TRUE)
mean_val_shifted <- mean(df$active_power_kw_shifted, na.rm = TRUE)

summary_table <- data.frame(
  Statistic = c("1st Quantile", "3rd Quantile", "5th Quantile", "Minimum", "Maximum", "Average", 
                "Log-Likelihood", "AIC", "BIC", "WAIC"),
  Value = c(
    quantiles[1],
    quantiles[2],
    quantiles[3],
    min_val_shifted,
    max_val_shifted,
    mean_val_shifted,
    logLik_value,
    AIC_value,
    BIC_value,
    WAIC_value
  )
)

cat("\n===== Summary Table =====\n")
print(summary_table, row.names = FALSE)
print(summary(poisson_model))
