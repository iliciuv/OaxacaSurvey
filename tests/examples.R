# Example usage of OaxacaSurvey

# Load required packages
library(survey)
library(boot)
library(OaxacaSurvey)

# Simulated sample size
n <- 1000
# Ensure reproductibility
set.seed(123)

# Define data object simulating a suvey with sampling weigths (variable w)
data <- data.frame(
  y = rnorm(n),
  x1 = rnorm(n),
  x2 = rnorm(n),
  group = ifelse(runif(n) > 0.5, 1, 0),
  w = round(runif(n, 1, 5))
)

# Apply "oaxaca_blinder_svy" function to simulated data
result <- oaxaca_blinder_svy(y ~ x1 + x2, data = data, group = "group", weights = "w")

# Return Oaxaca-Blinder decomposition with bootestraped CI
print(result)
