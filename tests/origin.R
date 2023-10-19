library(survey)
library(boot)

oaxaca_blinder_svy <- function(formula, data, group, weights,  R = 1000, conf.level = 0.95) {
  # Oaxaca-Blinder Decomposition on a single dataset
  single_decomposition <- function(data, indices) {
    data <- data[indices, ] # obtain the bootstrapped sample
    return(oaxaca_blinder_core(data, formula, group, weights))
  }

  # Core function without bootstrapping
  oaxaca_blinder_core <- function(data, formula, group, weights) {
    data1 <- data[data[[group]] == 1, ]
    data2 <- data[data[[group]] == 0, ]

    des1 <- svydesign(ids = ~1, data = data1, weights = ~ data1[[weights]])
    des2 <- svydesign(ids = ~1, data = data2, weights = ~ data2[[weights]])

    model1 <- svyglm(formula, design = des1)
    model2 <- svyglm(formula, design = des2)

# Extract decomposition
    endowments <-
    sum(coef(model1)[-1] * (colMeans(des2$variables[-1]) - colMeans(des1$variables[-1])))
    coefficients <-
    sum((coef(model2)[-1] - coef(model1)[-1]) * colMeans(des1$variables[-1]))
    interaction <-
    sum((coef(model2)[-1] - coef(model1)[-1]) * (colMeans(des2$variables[-1]) - colMeans(des1$variables[-1])))

# Return decomposition and bootestraped confidence intervals
    return(c(endowments, coefficients, interaction))
  }

  # Bootstrap
  set.seed(123) # for reproducibility
  boot.result <- boot(data = data, statistic = single_decomposition, R = R)

  # Compute Confidence Intervals
  alpha <- 1 - conf.level
  ci.lower <- apply(boot.result$t, 2, function(x) quantile(x, alpha / 2))
  ci.upper <- apply(boot.result$t, 2, function(x) quantile(x, 1 - alpha / 2))

  # Return results as a list
  result <- list(
    endowments = list(value = mean(boot.result$t[, 1]), CI = c(ci.lower[1], ci.upper[1])),
    coefficients = list(value = mean(boot.result$t[, 2]), CI = c(ci.lower[2], ci.upper[2])),
    interaction = list(value = mean(boot.result$t[, 3]), CI = c(ci.lower[3], ci.upper[3]))
  )
  return(result)
}

############################ Test code ############################

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
