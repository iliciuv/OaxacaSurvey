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
    endowments = results_mean[1],
    coefficients = results_mean[2],
    interaction = results_mean[3],
    ci = list(
      endowments = results_ci[1, ],
      coefficients = results_ci[2, ],
      interaction = results_ci[3, ]
    )
  )
}

decompose_core <- function(data, indices, formula, weights, group) {
  data_bootstrap <- data[indices, ]

  # Adjust weights for the bootstrap sample
  data_bootstrap[[weights]] <- ifelse(!is.na(data_bootstrap[[weights]]), data_bootstrap[[weights]], 0)

  data1 <- subset(data_bootstrap, data_bootstrap[[group]] == 1)
  data2 <- subset(data_bootstrap, data_bootstrap[[group]] == 0)

  des1 <- survey::svydesign(ids = ~1, data = data1, weights = ~ data1[[weights]])
  des2 <- survey::svydesign(ids = ~1, data = data2, weights = ~ data2[[weights]])

  model1 <- survey::svyglm(formula, design = des1)
  model2 <- survey::svyglm(formula, design = des2)

  lhs <- all.vars(formula)[1]
  meanY1 <- survey::svytotal(as.formula(paste0("~", lhs)), design = des1) / survey::svytotal(as.formula(paste0("~", data1[[weights]])), design = des1)
  meanY2 <- survey::svytotal(as.formula(paste0("~", lhs)), design = des2) / survey::svytotal(as.formula(paste0("~", data2[[weights]])), design = des2)

  diff <- meanY1[[1]] - meanY2[[1]]

  predicted1 <- predict(model1, type = "response", newdata = data1)
  predicted2 <- predict(model1, type = "response", newdata = data2)
  avg_predicted_diff <- mean(predicted2) - mean(predicted1)


  endowments <- sum(coef(model2)[-1]) * avg_predicted_diff
  coefficients <- sum(coef(model1)[-1] * predict(model2, type = "response", newdata = data1))
  interaction <- diff - endowments - coefficients

  return(c(endowments, coefficients, interaction))
}
