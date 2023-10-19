library(survey)
library(magrittr)

# Oaxaca-Blinder Decomposition using svyglm
oaxaca_blinder_svy <- function(formula, data, group, weights, subset=NULL) {
  # Split the data into two groups
  data1 <- data[data[[group]] == 1, ]
  data2 <- data[data[[group]] == 0, ]

  # Define survey designs
  des1 <- svydesign(ids = ~1, data = data1, weights = ~data1[[weights]])
  des2 <- svydesign(ids = ~1, data = data2, weights = ~data2[[weights]])

  # Fit models
  model1 <- svyglm(formula, design = des1)
  model2 <- svyglm(formula, design = des2)

  # Average outcomes
  lhs <- all.vars(formula)[1]
  meanY1 <- svytotal(as.formula(paste0("~", lhs)), design=des1) / svytotal(~data1[[weights]], design=des1)
  meanY2 <- svytotal(as.formula(paste0("~", lhs)), design=des2) / svytotal(~data2[[weights]], design=des2)

  diff <- meanY1 - meanY2

  # Decomposition
  predicted_diff <- predict(model1, type="response", newdata=data2) - predict(model1, type="response", newdata=data1)

  # Use the first element of the predicted difference since it's a vector
  endowments <- sum(coef(model2)[-1] * predicted_diff[1])

  coefficients <- sum(coef(model1)[-1] * predict(model2, type="response", newdata=data1))
  interaction <- diff - endowments - coefficients

  return(list(endowments = endowments, coefficients = coefficients, interaction = interaction))
}

# Test the function
n <- 1000
data <- data.frame(
  y = rnorm(n),
  x1 = rnorm(n),
  x2 = rnorm(n),
  group = ifelse(runif(n) > 0.5, 1, 0),
  w = round(runif(n, 1, 5))
)

result <- oaxaca_blinder_svy(y ~ x1 + x2, data = data, group = "group", weights = "w")
print(result)
