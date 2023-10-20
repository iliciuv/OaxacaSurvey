library(survey)
library(boot)
library(data.table)
library(magrittr)

oaxaca_blinder_svy <- function(formula, data, group, weights, R = 1000, conf.level = 0.95) {

  # Oaxaca-Blinder Decomposition on a single dataset

  single_decomposition <- function(data, indices) {
    data <- data[indices, ] # obtain the bootstrapped sample
    return(oaxaca_blinder_core(data, formula, group, weights))
  }

  weighted_means <- function(design, variables) {
    means <- sapply(variables, function(v) {
      as.numeric(svymean(as.formula(paste0("~ ", v)), design = design))
    })
    return(means)
  }

  # Core function without bootstrapping
  oaxaca_blinder_core <- function(data, formula, group, weights) {
    exclude_cols <- c("y", "group", "weights")

    data1 <- data[data$group == 1, ]
    data2 <- data[data$group == 0, ]

    des1 <- svydesign(ids = ~1, data = data1, weights = data1[, as.character(weights)])
    des2 <- svydesign(ids = ~1, data = data2, weights = data2[, as.character(weights)])

    model1 <- svyglm(formula, design = des1)
    model2 <- svyglm(formula, design = des2)

    relevant_vars <- names(des1$variables[!names(des1$variables) %in% exclude_cols])

    means1 <- weighted_means(des1, relevant_vars)
    means2 <- weighted_means(des2, relevant_vars)

    # Extract decomposition
    endowments <- sum(coef(model2)[-1] * (means1 - means2))
    coefficients <- sum((coef(model1)[-1] - coef(model2)[-1]) * means2)
    interaction <- sum((coef(model1)[-1] - coef(model2)[-1]) * (means1 - means2))

    # Return decomposition
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
    interaction = list(value = mean(boot.result$t[, 3]), CI = c(ci.lower[3], ci.upper[3])),
    total_effect = (mean(boot.result$t[, 1]) + mean(boot.result$t[, 2]) + mean(boot.result$t[, 3]))
  )
  return(result)
}

############################ Test code ############################

# Import dataset and prepare it for SurveyOaxaca
df <- fread("internal/eff-pool-2002-2020.csv")
df[, group := 0][class == "worker", group := 0][class == "capitalist", group := 1]
df[, rentsbi := 0][rents >= renthog * 0.1 & rents > 2000, rentsbi := 1]

data <- data.frame(
  y = df$renthog,
  x1 = df$rentsbi,
  x2 = as.numeric(as.factor(df$homeowner)) - 2,
  group = df$group,
  weights = df$facine3
)

result <- oaxaca_blinder_svy(
  y ~ x1 + x2,
  data = data,
  group = "group",
  weights = "weights",
  R = 10
)














####################################################################################

  weighted_means <- function(design, variables) {
    means <- sapply(variables, function(v) {
      as.numeric(svymean(as.formula(paste0("~ ", v)), design = design))
    })
    return(means)
  }

  # Core function without bootstrapping
  oaxaca_blinder_core <- function(data, formula, group, weights) {
    exclude_cols <- c("y", "group", "weights")

    data1 <- data[data$group == 1, ]
    data2 <- data[data$group == 0, ]

    des1 <- svydesign(ids = ~1, data = data1, weights = data1[, as.character(weights)])
    des2 <- svydesign(ids = ~1, data = data2, weights = data2[, as.character(weights)])

    model1 <- svyglm(formula, design = des1)
    model2 <- svyglm(formula, design = des2)

    relevant_vars <- names(des1$variables[!names(des1$variables) %in% exclude_cols])

    means1 <- weighted_means(des1, relevant_vars)
    means2 <- weighted_means(des2, relevant_vars)

    # Extract decomposition
    endowments <- sum(coef(model2)[-1] * (means1 - means2))
    coefficients <- sum((coef(model1)[-1] - coef(model2)[-1]) * means2)
    interaction <- sum((coef(model1)[-1] - coef(model2)[-1]) * (means1 - means2))

    # Return decomposition
    return(c(endowments, coefficients, interaction))
  }

exclude_cols <- c("y", "group", "weights")
weights <- "weights"
group <- "group"
formula <- y ~ x1 + x2
result2 <- oaxaca_blinder_core(
  y ~ x1 + x2,
  data = data,
  group = "group",
  weights = "weights"
)
result2 %>% print()
"####################################################" %>% print()
result %>% print()


data1 <- data[data$group == 1, ]
data2 <- data[data$group == 0, ]

des1 <- svydesign(ids = ~1, data = data1, weights = data1[, as.character(weights)])
des2 <- svydesign(ids = ~1, data = data2, weights = data2[, as.character(weights)])

model1 <- svyglm(formula, design = des1)
model2 <- svyglm(formula, design = des2)
