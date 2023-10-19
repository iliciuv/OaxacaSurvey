# Beta module OaxacaSurvey

#' Oaxaca-Blinder Decomposition with Bootstrap Confidence Intervals using svyglm
#' @import boot
#' @import survey
#' @param formula An object of class formula: the model formula.
#' @param data A data frame containing the variables in the formula, group, and weights.
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

  # Core function without bootstrapping
  oaxaca_blinder_core <- function(data, formula, group, weights) {
    data1 <- data[data[[group]] == 1, ]
    data2 <- data[data[[group]] == 0, ]

    des1 <- survey::svydesign(ids = ~1, data = data1, weights = ~ data1[[weights]])
    des2 <- survey::svydesign(ids = ~1, data = data2, weights = ~ data2[[weights]])

    model1 <- survey::svyglm(formula, design = des1)
    model2 <- survey::svyglm(formula, design = des2)

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
  boot.result <- boot::boot(data = data, statistic = single_decomposition, R = R)

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
