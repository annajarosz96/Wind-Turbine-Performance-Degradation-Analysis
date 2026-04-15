library(MASS)
library(ggplot2)
library(readxl)
library(gridExtra)

# Read the data from the Excel file
data <- read_excel("Dataset.xlsx")

# Create the data frame with new columns
df <- data.frame(
  datetime = data[[1]],
  active_power_kw = data[[2]],
  wind_speed_ms = data[[3]],
  theoretical_power_kwh = data[[4]],
  wind_direction_deg = data[[5]]
)

# Plot regression with centered title
plot_ap_ws <- ggplot(df, aes(x = wind_speed_ms, y = active_power_kw)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Linear Regression Model",
       x = "Wind Speed (m/s)",
       y = "Active Power (kW)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

print(plot_ap_ws)

# Fit linear regression: Active Power ~ Wind Speed
lm_ap_ws <- lm(active_power_kw ~ wind_speed_ms, data = df)

# Extract model info
logLik_value <- as.numeric(logLik(lm_ap_ws)) # log-likelihood
k <- length(coef(lm_ap_ws))                  # number of parameters
n <- nrow(df)                                # number of observations

# Calculate AIC and BIC using formulas
AIC_value <- 2 * k - 2 * logLik_value        # Akaike Information Criterion
BIC_value <- k * log(n) - 2 * logLik_value   # Bayesian Information Criterion

# WAIC approximation (for illustration, not for publication)
WAIC_value <- -2 * logLik_value

# Descriptive statistics for active_power_kw
quantiles <- quantile(df$active_power_kw, probs = c(0.01, 0.03, 0.05))
min_val <- min(df$active_power_kw, na.rm = TRUE)
max_val <- max(df$active_power_kw, na.rm = TRUE)
mean_val <- mean(df$active_power_kw, na.rm = TRUE)

# Create summary table
summary_table <- data.frame(
  Statistic = c("1st Quantile", "3rd Quantile", "5th Quantile", "Minimum", "Maximum", "Average", 
                "Log-Likelihood", "AIC", "BIC", "WAIC"),
  Value = c(
    quantiles[1],
    quantiles[2],
    quantiles[3],
    min_val,
    max_val,
    mean_val,
    logLik_value,
    AIC_value,
    BIC_value,
    WAIC_value
  )
)

print(summary_table)
