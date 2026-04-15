# Load required libraries
library(readxl)
library(gridExtra)
library(ggplot2)
library(coda)
library(MASS)

# Replace "path/to/Dataset.xlsx" with the actual file path
data <- read_excel("Dataset.xlsx")

# Combine the extracted data into a single data frame
df <- data.frame(
  voltage_phase_1 = data[[3]],
  voltage_phase_2 = data[[4]],
  voltage_phase_3 = data[[5]],
  current_phase_1 = data[[6]],
  current_phase_2 = data[[7]],
  current_phase_3 = data[[8]],
  active_power_phase_1 = data[[12]],
  active_power_phase_2 = data[[13]],
  active_power_phase_3 = data[[14]]
)

# Create a new variable for power decrease based on voltage increase
df$power_decrease <- (df$active_power_phase_1 + df$active_power_phase_2 + df$active_power_phase_3) / (df$voltage_phase_1 + df$voltage_phase_2 + df$voltage_phase_3)

# Generalized Linear Regression model
glm_model <- glm(power_decrease ~ voltage_phase_1 + voltage_phase_2 + voltage_phase_3 +
                   current_phase_1 + current_phase_2 + current_phase_3 +
                   active_power_phase_1 + active_power_phase_2 + active_power_phase_3,
                 data = df, family = gaussian(link = "identity"))

# Obtain the summary of the model
summary(glm_model)

# Extract coefficients and quantities
coefficients <- coef(glm_model)
quantiles <- quantile(coefficients, probs = c(0, 0.25, 0.5, 0.75, 1))

# Obtain AIC and BIC
aic <- AIC(glm_model)
bic <- BIC(glm_model)

# Prior distribution plot with increased values
prior_df <- data.frame(x = seq(-10, 10, length.out = 100), 
                       y = dnorm(seq(-10, 10, length.out = 100), mean(df$power_decrease), sd(df$power_decrease) * 2))  # Increased standard deviation for illustration
prior_plot <- ggplot(prior_df, aes(x, y)) +
  geom_line(color = "blue") +
  labs(title = "Prior Distribution (Increased Values)")

# Likelihood plot
likelihood_df <- data.frame(x = 1:length(glm_model$residuals), y = glm_model$residuals)
likelihood_plot <- ggplot(likelihood_df, aes(x, y)) +
  geom_point(color = "mediumblue") +
  labs(title = "Likelihood")

# Generate posterior samples
posterior_samples <- mcmc(as.matrix(model.matrix(glm_model)))

# Calculate the posterior mean and standard deviation
posterior_mean <- colMeans(posterior_samples)
posterior_sd <- apply(posterior_samples, 2, sd)

# Create a data frame for the posterior distribution
posterior_df <- data.frame(x = seq(-10, 10, length.out = 100), 
                           y = dnorm(seq(-10, 10, length.out = 100), posterior_mean, posterior_sd))
posterior_plot <- ggplot(posterior_df, aes(x, y)) +
  geom_line(color = "darkblue") +
  labs(title = "Posterior Distribution (Gaussian)")

# Show all plots side by side and print the precision values
grid.arrange(prior_plot, likelihood_plot, posterior_plot, ncol = 3)
print(summary(glm_model)$coefficients[, c("Estimate", "Std. Error")])