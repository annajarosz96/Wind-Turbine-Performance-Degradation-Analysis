
library(randomForest)
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
  multiphase_voltage_1_2 = data[[9]],
  multiphase_voltage_2_3 = data[[10]],
  multiphase_voltage_3_1 = data[[11]],
  current_phase_1 = data[[6]],
  current_phase_2 = data[[7]],
  current_phase_3 = data[[8]],
  active_power_phase_1 = data[[12]],
  active_power_phase_2 = data[[13]],
  active_power_phase_3 = data[[14]]
)

# Create a new variable for power decrease based on voltage increase
df$power_decrease <- df$current_phase_1 * df$voltage_phase_1 + df$current_phase_2 * df$voltage_phase_2 + df$current_phase_3 * df$voltage_phase_3

# Fit a Random Forest model
rf_model <- randomForest(power_decrease ~ ., data = df)

# Extract variable importance
var_importance <- importance(rf_model)

# Obtain the summary of the model
summary_rf_model <- summary(rf_model)

# Extract coefficients and quantities
# As random forest does not provide coefficients, we focus on variable importance and other relevant information

# Calculate Min, 1Q, Median, 3Q, Max for variable importance
quantiles_var_importance <- quantile(var_importance$MeanDecreaseGini, probs = c(0, 0.25, 0.5, 0.75, 1))

# Print the results
print("Variable Importance:")
print(var_importance)
print("Quantiles for Variable Importance:")
print(quantiles_var_importance)
print("Summary of the Random Forest Model:")
print(summary_rf_model)

# Plot the variable importance
var_importance_plot <- ggplot(data = var_importance, aes(x = reorder(rownames(var_importance), -MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity") +
  labs(x = "Variable", y = "Mean Decrease in Gini Impurity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Show the variable importance plot
print(var_importance_plot)