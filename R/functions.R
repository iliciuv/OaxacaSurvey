# Beta module OaxacaSurvey

library(survey)
library(BH)

#' Oaxaca-Blinder Decomposition with Bootstrap Confidence Intervals using svyglm
#'
#' @param formula An object of class formula: the model formula.
#' @param data A data frame containing the variables in the formula, group, and weights.
#' @param group A character string: the name of the binary group variable in the data. 1 for the first group, and 0 for the second group.
#' @param weights A character string: the name of the weights variable in the data.
#' @param R An integer: the number of bootstrap replicates. Default is 1000.
#'
#' @return A list containing the mean and confidence intervals for endowments, coefficients, and interaction components.
#'
#' @examples
#' \dontrun{
#' # Assuming a dataset 'data' with response 'y', predictors 'x1' and 'x2', a binary group 'group', and weights 'w'
#' result <- oaxaca_blinder_svy(y ~ x1 + x2, data = data, group = "group", weights = "w", R = 1000)
#' }
oaxaca_blinder_svy <- function(formula, data, group, weights, R=1000) {

  # Core decomposition function for bootstrapping
  decompose_core <- function(data, indices) {
    data_bootstrap <- data[indices, ]
    data1 <- subset(data_bootstrap, get(group) == 1)
    data2 <- subset(data_bootstrap, get(group) == 0)

    des1 <- svydesign(ids = ~1, data = data1, weights = get(weights))
    des2 <- svydesign(ids = ~1, data = data2, weights = get(weights))

    model1 <- svyglm(formula, design = des1)
    model2 <- svyglm(formula, design = des2)

    meanY1 <- svytotal(~response, design=des1) / svytotal(get(weights), design=des1)
    meanY2 <- svytotal(~response, design=des2) / svytotal(get(weights), design=des2)

    diff <- meanY1 - meanY2

    endowments <- as.numeric(coef(model2)[-1] %*% (predict(model1, type="response", newdata=data2) - predict(model1, type="response", newdata=data1)))
    coefficients <- as.numeric(coef(model1)[-1] %*% predict(model2, type="response", newdata=data1))
    interaction <- diff - endowments - coefficients

    return(c(endowments, coefficients, interaction))
  }

  # Bootstrap for confidence intervals
  results <- boot(data=data, statistic=decompose_core, R=R)

  # Extract results: mean and CI
  results_mean <- apply(results$t, 2, mean)
  results_ci <- t(apply(results$t, 2, function(x) quantile(x, c(0.025, 0.975))))

  list(
    endowments = results_mean[1], coefficients = results_mean[2], interaction = results_mean[3],
    ci = list(
      endowments = results_ci[1, ], coefficients = results_ci[2, ], interaction = results_ci[3, ]
    )
  )
}
