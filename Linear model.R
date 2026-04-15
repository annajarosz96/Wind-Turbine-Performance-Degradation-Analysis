# Load required libraries
library(MASS)
library(ggplot2)
library(readxl)
library(gridExtra)

# Read the data from the Excel file
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

# Scale the power decrease to the range 0.0-0.4
df$power_decrease <- (df$current_phase_1 * df$voltage_phase_1 + 
                        df$current_phase_2 * df$voltage_phase_2 + 
                        df$current_phase_3 * df$voltage_phase_3) / 
  max(df$current_phase_1 * df$voltage_phase_1 + 
        df$current_phase_2 * df$voltage_phase_2 + 
        df$current_phase_3 * df$voltage_phase_3) * 0.4

# Fit GLMs for each phase with linear terms
glm_phase_1 <- glm(power_decrease ~ voltage_phase_1, 
                   data = df, family = gaussian(link = "identity"))
glm_phase_2 <- glm(power_decrease ~ voltage_phase_2, 
                   data = df, family = gaussian(link = "identity"))
glm_phase_3 <- glm(power_decrease ~ voltage_phase_3, 
                   data = df, family = gaussian(link = "identity"))

# Extract coefficients for each phase
coefficients_phase_1 <- coef(glm_phase_1)
coefficients_phase_2 <- coef(glm_phase_2)
coefficients_phase_3 <- coef(glm_phase_3)

# Print coefficients to observe differences
print("Coefficients for Phase 1:")
print(coefficients_phase_1)
print("Coefficients for Phase 2:")
print(coefficients_phase_2)
print("Coefficients for Phase 3:")
print(coefficients_phase_3)

# Plot the relationship between voltage and power decrease for each phase
plot_phase_1 <- ggplot(df, aes(x = voltage_phase_1, y = power_decrease)) +
  geom_point(color = "black", alpha = 0.6) +
  geom_smooth(method = "glm", formula = y ~ x, 
              method.args = list(family = gaussian(link = "identity")), 
              se = TRUE, color = "blue") +
  labs(title = paste("Phase 1: Voltage vs Power Decrease\n",
                     "Coeff: Intercept =", round(coefficients_phase_1[1], 3), 
                     ", Slope =", round(coefficients_phase_1[2], 3)),
       x = "Voltage Phase 1", y = "Power Decrease") +
  theme_minimal()

plot_phase_2 <- ggplot(df, aes(x = voltage_phase_2, y = power_decrease)) +
  geom_point(color = "black", alpha = 0.6) +
  geom_smooth(method = "glm", formula = y ~ x, 
              method.args = list(family = gaussian(link = "identity")), 
              se = TRUE, color = "blue") +
  labs(title = paste("Phase 2: Voltage vs Power Decrease\n",
                     "Coeff: Intercept =", round(coefficients_phase_2[1], 3), 
                     ", Slope =", round(coefficients_phase_2[2], 3)),
       x = "Voltage Phase 2", y = "Power Decrease") +
  theme_minimal()

plot_phase_3 <- ggplot(df, aes(x = voltage_phase_3, y = power_decrease)) +
  geom_point(color = "black", alpha = 0.6) +
  geom_smooth(method = "glm", formula = y ~ x, 
              method.args = list(family = gaussian(link = "identity")), 
              se = TRUE, color = "blue") +
  labs(title = paste("Phase 3: Voltage vs Power Decrease\n",
                     "Coeff: Intercept =", round(coefficients_phase_3[1], 3), 
                     ", Slope =", round(coefficients_phase_3[2], 3)),
       x = "Voltage Phase 3", y = "Power Decrease") +
  theme_minimal()

# Arrange the plots in a grid
grid.arrange(plot_phase_1, plot_phase_2, plot_phase_3, ncol = 3)

