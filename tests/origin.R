library(survey)
library(boot)
library(data.table)


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

    # Split 2 distinct control groups
    data1 <- data[data$group == 1, ]
    data2 <- data[data$group == 0, ]

    # Define survey design accounting for sample weights and other characteristics
    des1 <- svydesign(ids = ~1, data = data1, weights = data1[, as.character(weights)])
    des2 <- svydesign(ids = ~1, data = data2, weights = data2[, as.character(weights)])

    # Estimate svygml model accounting for survey design
    model1 <- svyglm(formula, design = des1)
    model2 <- svyglm(formula, design = des2)

    # Obtain weighted means for needed variables
    relevant_vars <- names(des1$variables[!names(des1$variables) %in% exclude_cols])
    means1 <- weighted_means(des1, relevant_vars)
    means2 <- weighted_means(des2, relevant_vars)
    means1_y <- weighted_means(des1, "y")
    means2_y <- weighted_means(des2, "y")

    # Extract decomposition
    endowments <- sum(coef(model2)[-1] * (means1 - means2))
    coefficients <- sum((coef(model1)[-1] - coef(model2)[-1]) * means2)
    interaction <- sum((coef(model1)[-1] - coef(model2)[-1]) * (means1 - means2))
    unexplained <- unname(coef(model1)[1] - coef(model2)[1])
    total <- endowments + coefficients + interaction + unexplained

    # Return decomposition
    return(
      c(
        unex = unexplained, end = endowments, coef = coefficients, inter = interaction, total = total,
        means1 = means1_y, means2 = means2_y, means_dif = (means1_y - means2_y)
      )
    )
  }

  # Bootstrap
  set.seed(123) # for reproducibility
  boot.result <- boot(data = data, statistic = single_decomposition, R = R)

  # Compute Confidence Intervals
  alpha <- 1 - conf.level
  ci.lower <- apply(boot.result$t, 2, function(x) quantile(x, alpha / 2))
  ci.upper <- apply(boot.result$t, 2, function(x) quantile(x, 1 - alpha / 2))

  # Return results as a list
  result <- data.table::data.table(
    unex = list(value = mean(boot.result$t[, 1]), CI = c(ci.lower[1], ci.upper[1])),
    end = list(value = mean(boot.result$t[, 2]), CI = c(ci.lower[2], ci.upper[2])),
    coef = list(value = mean(boot.result$t[, 3]), CI = c(ci.lower[3], ci.upper[3])),
    inter = list(value = mean(boot.result$t[, 4]), CI = c(ci.lower[4], ci.upper[4])),
    total = list(value = mean(boot.result$t[, 5]), CI = c(ci.lower[5], ci.upper[5])),
    means1_y = list(value = mean(boot.result$t[, 6]), CI = c(ci.lower[6], ci.upper[6])),
    means2_y = list(value = mean(boot.result$t[, 7]), CI = c(ci.lower[7], ci.upper[7])),
    means_dif = list(value = mean(boot.result$t[, 8]), CI = c(ci.lower[8], ci.upper[8]))
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


result %>% print()
