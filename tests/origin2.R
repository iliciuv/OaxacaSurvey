library(survey)
library(magrittr)
library(boot)

# Core Oaxaca-Blinder Decomposition function
decompose_core <- function(data, indices) {
  data_sample <- data[indices, ]

  formula <- y ~ x1 + x2
  group <- "group"
  weights <- "w"

  data1 <- data_sample[data_sample[[group]] == 1, ]
  data2 <- data_sample[data_sample[[group]] == 0, ]

  des1 <- svydesign(ids = ~1, data = data1, weights = ~data1[[weights]])
  des2 <- svydesign(ids = ~1, data = data2, weights = ~data2[[weights]])

  model1 <- svyglm(formula, design = des1)
  model2 <- svyglm(formula, design = des2)

  lhs <- all.vars(formula)[1]
  meanY1 <- svytotal(as.formula(paste0("~", lhs)), design=des1) / svytotal(~data1[[weights]], design=des1)
  meanY2 <- svytotal(as.formula(paste0("~", lhs)), design=des2) / svytotal(~data2[[weights]], design=des2)

  diff <- meanY1 - meanY2

  predicted_diff <- predict(model1, newdata=data2) - predict(model1, newdata=data1)
  endowments <- sum(coef(model2)[-1] * predicted_diff[1])
  coefficients <- sum(coef(model1)[-1] * predict(model2, newdata=data1))
  interaction <- diff - endowments - coefficients

  return(c(endowments, coefficients, interaction))
}

# Bootstrap confidence intervals
oaxaca_blinder_svy_bootstrap <- function(data) {
  set.seed(123)
  results <- boot(data, decompose_core, R = 500)

  ci_endowments <- boot.ci(results, type="perc", index=1)$percent
  ci_coefficients <- boot.ci(results, type="perc", index=2)$percent
  ci_interaction <- boot.ci(results, type="perc", index=3)$percent

  list(
    endowments = list(mean = mean(results$t[,1]), ci = ci_endowments),
    coefficients = list(mean = mean(results$t[,2]), ci = ci_coefficients),
    interaction = list(mean = mean(results$t[,3]), ci = ci_interaction)
  )
}

# Test data
n <- 1000
data <- data.frame(
  y = rnorm(n),
  x1 = rnorm(n),
  x2 = rnorm(n),
  group = ifelse(runif(n) > 0.5, 1, 0),
  w = round(runif(n, 1, 5))
)

# Results
result <- oaxaca_blinder_svy_bootstrap(data)
print(result)
