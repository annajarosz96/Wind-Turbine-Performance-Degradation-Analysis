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

# Fit quadratic regression model: y = b0 + b1*x + b2*x^2
quad_model <- lm(active_power_kw ~ wind_speed_ms + I(wind_speed_ms^2), data = df)

# Create a sequence of wind speeds for smooth curve plotting
wind_speed_seq <- seq(
  min(df$wind_speed_ms, na.rm = TRUE),
  max(df$wind_speed_ms, na.rm = TRUE),
  length.out = 200
)

# Predict active power using the quadratic model for the sequence
predicted_power <- predict(
  quad_model,
  newdata = data.frame(wind_speed_ms = wind_speed_seq)
)

# Create a data frame for predicted values
pred_df <- data.frame(
  wind_speed_ms = wind_speed_seq,
  predicted_power = predicted_power
)

# Plot points and quadratic regression curve
plot_quad <- ggplot(df, aes(x = wind_speed_ms, y = active_power_kw)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_line(data = pred_df, aes(x = wind_speed_ms, y = predicted_power), color = "black", size = 1.2) +
  labs(title = "Quadratic Regression Model",
       x = "Wind Speed (m/s)",
       y = "Active Power (kW)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(plot_quad)

# Extract model info
logLik_value <- as.numeric(logLik(quad_model))
k <- length(coef(quad_model))
n <- nrow(df)
AIC_value <- 2 * k - 2 * logLik_value
BIC_value <- k * log(n) - 2 * logLik_value
WAIC_value <- -2 * logLik_value

# Descriptive statistics for active_power_kw
quantiles <- quantile(df$active_power_kw, probs = c(0.01, 0.03, 0.05), na.rm = TRUE)
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

cat("\n===== Summary Table =====\n")
print(summary_table, row.names = FALSE)
print(summary(quad_model))
