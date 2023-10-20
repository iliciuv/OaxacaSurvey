# Beta module OaxacaSurvey

#' Oaxaca-Blinder Decomposition with Bootstrap Confidence Intervals using svyglm
#' @import boot
#' @import survey
#' @param formula An object of class formula: the model formula.
#' @param data A data frame: containing the variables in the formula, group, and weights.
#' @param group A character string: the name of the binary group variable in the data. 1 for the first group, and 0 for the second group.
#' @param weights A character string: the name of the weights variable in the data.
#' @param R An integer: the number of bootstrap replicates. Default is 1000.
#' @param conf.level A float less than 1: Confidence level for estimated CI.
#'
#' @return A list containing the mean and confidence intervals for endowments, coefficients, and interaction components.
#' @examples
#' \dontrun{
#' # Assuming a dataset 'data' with response 'y', predictors 'x1' and 'x2', a binary group 'group', and weights 'w'
#' result <- oaxaca_blinder_svy(y ~ x1 + x2, data = data, group = "group", weights = "w", R = 1000)
#' }
#' @export
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
      list(
        oaxaca_blinder = c(unex = unexplained, end = endowments, coef = coefficients, inter = interaction, total = total),
        mean_values_endo = c(means1 = means1_y, means2 = means2_y, means_dif = (means1_y - means2_y))
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
  result <- list(
    endowments = list(value = mean(boot.result$t[, 1]), CI = c(ci.lower[1], ci.upper[1])),
    coefficients = list(value = mean(boot.result$t[, 2]), CI = c(ci.lower[2], ci.upper[2])),
    interaction = list(value = mean(boot.result$t[, 3]), CI = c(ci.lower[3], ci.upper[3])),
    total_effect = (mean(boot.result$t[, 1]) + mean(boot.result$t[, 2]) + mean(boot.result$t[, 3]))
  )
  return(result)
}
