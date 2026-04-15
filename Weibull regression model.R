# Load required libraries
library(ggplot2)
library(readxl)
library(gridExtra)
library(lme4)
library(randomForest)
library(rstanarm)
library(loo)

# Read the data from the Excel file
data <- read_excel("Dataset.xlsx")

# Create the data frame
df <- data.frame(
  datetime = data[[1]],
  active_power_kw = data[[2]],
  wind_speed_ms = data[[3]],
  theoretical_power_kwh = data[[4]],
  wind_direction_deg = as.factor(data[[5]])
)

# Remove rows with missing values for plotting/modeling
df <- df[!is.na(df$active_power_kw) & !is.na(df$wind_speed_ms), ]

# Sequence for smooth curves
wind_speed_seq <- seq(min(df$wind_speed_ms), max(df$wind_speed_ms), length.out = 200)

# Helper function for summary table
make_summary_table <- function(y, model, logLik_value, AIC_value, BIC_value, WAIC_value) {
  quantiles <- quantile(y, probs = c(0.01, 0.03, 0.05), na.rm = TRUE)
  min_val <- min(y, na.rm = TRUE)
  max_val <- max(y, na.rm = TRUE)
  mean_val <- mean(y, na.rm = TRUE)
  data.frame(
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
}

# --- GLM: Linear relationship ---
glm_model <- glm(active_power_kw ~ wind_speed_ms, data = df, family = gaussian())
pred_glm <- predict(glm_model, newdata = data.frame(wind_speed_ms = wind_speed_seq), type = "response")
pred_glm_df <- data.frame(wind_speed_ms = wind_speed_seq, predicted = pred_glm)
plot_glm <- ggplot(df, aes(x = wind_speed_ms, y = active_power_kw)) +
  geom_point(color = "grey40", alpha = 0.4) +
  geom_line(data = pred_glm_df, aes(x = wind_speed_ms, y = predicted), color = "blue", size = 1.2) +
  labs(title = "GLM (Linear)", x = "Wind Speed (m/s)", y = "Active Power (kW)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

logLik_value <- as.numeric(logLik(glm_model))
AIC_value <- AIC(glm_model)
BIC_value <- BIC(glm_model)
WAIC_value <- NA
summary_table_glm <- make_summary_table(df$active_power_kw, glm_model, logLik_value, AIC_value, BIC_value, WAIC_value)

cat("\n===== GLM (Linear) Summary Table =====\n")
print(summary_table_glm, row.names = FALSE)
print(summary(glm_model))

# --- GLMM: Quadratic relationship (random intercept by wind_direction_deg) ---
glmm_model <- lmer(active_power_kw ~ poly(wind_speed_ms, 2) + (1 | wind_direction_deg), data = df)
pred_glmm <- predict(glmm_model, newdata = data.frame(
  wind_speed_ms = wind_speed_seq,
  wind_direction_deg = levels(df$wind_direction_deg)[1]
), allow.new.levels = TRUE)
pred_glmm_df <- data.frame(wind_speed_ms = wind_speed_seq, predicted = pred_glmm)
plot_glmm <- ggplot(df, aes(x = wind_speed_ms, y = active_power_kw)) +
  geom_point(color = "grey40", alpha = 0.4) +
  geom_line(data = pred_glmm_df, aes(x = wind_speed_ms, y = predicted), color = "red", size = 1.2) +
  labs(title = "GLMM (Quadratic)", x = "Wind Speed (m/s)", y = "Active Power (kW)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

logLik_value <- as.numeric(logLik(glmm_model))
AIC_value <- AIC(glmm_model)
BIC_value <- BIC(glmm_model)
WAIC_value <- NA
summary_table_glmm <- make_summary_table(df$active_power_kw, glmm_model, logLik_value, AIC_value, BIC_value, WAIC_value)

cat("\n===== GLMM (Quadratic) Summary Table =====\n")
print(summary_table_glmm, row.names = FALSE)
print(summary(glmm_model))

# --- Random Forest Regression (for completeness, not a regression line) ---
set.seed(42)
rf_model <- randomForest(active_power_kw ~ wind_speed_ms, data = df, ntree = 100)
pred_rf <- predict(rf_model, newdata = data.frame(wind_speed_ms = wind_speed_seq))
pred_rf_df <- data.frame(wind_speed_ms = wind_speed_seq, predicted = pred_rf)
plot_rf <- ggplot(df, aes(x = wind_speed_ms, y = active_power_kw)) +
  geom_point(color = "grey40", alpha = 0.4) +
  geom_line(data = pred_rf_df, aes(x = wind_speed_ms, y = predicted), color = "orange", size = 1.2) +
  labs(title = "Random Forest", x = "Wind Speed (m/s)", y = "Active Power (kW)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

summary_table_rf <- make_summary_table(df$active_power_kw, rf_model, NA, NA, NA, NA)
cat("\n===== Random Forest Summary Table =====\n")
print(summary_table_rf, row.names = FALSE)
print(rf_model)

# --- MCMC: Cubic relationship (Bayesian regression) ---
set.seed(42)
mcmc_model <- stan_glm(active_power_kw ~ poly(wind_speed_ms, 3), data = df, family = gaussian(), refresh = 0, iter = 1000, chains = 2)
pred_mcmc <- posterior_linpred(mcmc_model, newdata = data.frame(wind_speed_ms = wind_speed_seq), transform = TRUE)
pred_mcmc_mean <- apply(pred_mcmc, 2, mean)
pred_mcmc_df <- data.frame(wind_speed_ms = wind_speed_seq, predicted = pred_mcmc_mean)
plot_mcmc <- ggplot(df, aes(x = wind_speed_ms, y = active_power_kw)) +
  geom_point(color = "grey40", alpha = 0.4) +
  geom_line(data = pred_mcmc_df, aes(x = wind_speed_ms, y = predicted), color = "darkgreen", size = 1.2) +
  labs(title = "MCMC (Cubic)", x = "Wind Speed (m/s)", y = "Active Power (kW)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

logLik_value <- sum(apply(log_lik(mcmc_model), 2, mean))
AIC_value <- NA
BIC_value <- NA
WAIC_value <- loo(mcmc_model)$estimates["waic", "Estimate"]
summary_table_mcmc <- make_summary_table(df$active_power_kw, mcmc_model, logLik_value, AIC_value, BIC_value, WAIC_value)

cat("\n===== MCMC (Cubic) Summary Table =====\n")
print(summary_table_mcmc, row.names = FALSE)
print(summary(mcmc_model))

# --- Arrange all plots in a 2x2 grid ---
grid.arrange(plot_glm, plot_glmm, plot_rf, plot_mcmc, nrow = 2)
