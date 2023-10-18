# Example usage package OaxacaSurvey

# Load required packages
library(survey)
library(BH)
library(OaxacaSurvey)

# Source the oaxaca_blinder_svy function here if it's not already loaded

# Set seed for replicability
set.seed(123)

# Simulate some data
n <- 1000

data <- data.frame(
  y = rnorm(n),
  x1 = rnorm(n),
  x2 = rnorm(n),
  group = ifelse(runif(n) > 0.5, 1, 0),
  w = round(runif(n, 1, 5))
)

# Add some effect for group
data$y[data$group == 1] <- data$y[data$group == 1] + 0.5

# Testing the function
result <- oaxaca_blinder_svy(y ~ x1 + x2, data = data, group = "group", weights = "w", R = 500)

# Print the results
print(result)
