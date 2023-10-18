# Beta module OaxacaSurvey

#' Oaxaca-Blinder Decomposition with Bootstrap Confidence Intervals using svyglm
#' @import resample
#' @import survey
#' @param formula An object of class formula: the model formula.
#' @param data A data frame containing the variables in the formula, group, and weights.
#' @param group A character string: the name of the binary group variable in the data. 1 for the first group, and 0 for the second group.
#' @param weights A character string: the name of the weights variable in the data.
#' @param R An integer: the number of bootstrap replicates. Default is 1000.
#'
#' @return A list containing the mean and confidence intervals for endowments, coefficients, and interaction components.
#' @examples
#' \dontrun{
#' # Assuming a dataset 'data' with response 'y', predictors 'x1' and 'x2', a binary group 'group', and weights 'w'
#' result <- oaxaca_blinder_svy(y ~ x1 + x2, data = data, group = "group", weights = "w", R = 1000)
#' }
#' @export
oaxaca_blinder_svy <- function(formula, data, group, weights, R = 1000) {
  # Bootstrap for confidence intervals
  bootstrap_results <- resample::bootstrap(data, function(data, indices) {
    decompose_core(data, indices, formula, weights, group)
  }, R = R)

  # Extract results: mean and CI
  results_mean <- colMeans(bootstrap_results)
  results_ci <- t(apply(bootstrap_results, 2, function(x) quantile(x, c(0.025, 0.975))))

  list(
    endowments = results_mean[1], coefficients = results_mean[2], interaction = results_mean[3],
    ci = list(
      endowments = results_ci[1, ], coefficients = results_ci[2, ], interaction = results_ci[3, ]
    )
  )
}

decompose_core <- function(data, indices, formula, weights, group) {
  data_bootstrap <- data[indices, ]
  data1 <- subset(data_bootstrap, get(group) == 1)
  data2 <- subset(data_bootstrap, get(group) == 0)
  # Adjust weights for the bootstrap sample
  data_bootstrap$w <- ifelse(!is.na(data_bootstrap$w), data_bootstrap$w, 0)

  des1 <- survey::svydesign(ids = ~1, data = data1, weights = get(weights))
  des2 <- survey::svydesign(ids = ~1, data = data2, weights = get(weights))

  model1 <- survey::svyglm(formula, design = des1)
  model2 <- survey::svyglm(formula, design = des2)

  meanY1 <- survey::svytotal(~response, design = des1) / survey::svytotal(get(weights), design = des1)
  meanY2 <- survey::svytotal(~response, design = des2) / survey::svytotal(get(weights), design = des2)

  diff <- meanY1 - meanY2

  endowments <- as.numeric(coef(model2)[-1] %*% (predict(model1, type = "response", newdata = data2) - predict(model1, type = "response", newdata = data1)))
  coefficients <- as.numeric(coef(model1)[-1] %*% predict(model2, type = "response", newdata = data1))
  interaction <- diff - endowments - coefficients

  return(c(endowments, coefficients, interaction))
}
