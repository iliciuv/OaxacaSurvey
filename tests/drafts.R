library(survey)
library(boot)
library(data.table)
library(magrittr)


############################ Test code ############################

# Import dataset and prepare it for SurveyOaxaca
df <- fread("internal/eff-pool-2002-2020.csv")
df[, group := 0][class == "worker", group := 0][class == "capitalist", group := 1]
df[, rentsbi := 0][rents >= renthog * 0.1 & rents > 2000, rentsbi := 1]

data <- data.frame(
  y = df$renthog,
  x1 = as.numeric(as.factor(df$sex)),
  x2 = df$riquezanet,
  group = df$group,
  weights = df$facine3
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
  unexplained <- unname(coef(model1)[1] - coef(model2)[1])
  total <- endowments + coefficients + interaction + unexplained
  # Return decomposition
  return(c(unex = unexplained, end = endowments, coef = coefficients, inter = interaction, total = total))
}


################################################################################# 3

exclude_cols <- c("y", "group", "weights")
weights <- "weights"
group <- "group"
formula <- y ~ x1 + x2
result <- oaxaca_blinder_core(
  y ~ x1 + x2,
  data = data,
  group = "group",
  weights = "weights"
)
result %>% print()

data1 <- data[data$group == 1, ]
data2 <- data[data$group == 0, ]
data2 <- sample(data2, nrow(data1), replace = TRUE)

des1 <- svydesign(ids = ~1, data = data1, weights = data1[, as.character(weights)])
des2 <- svydesign(ids = ~1, data = data2, weights = data2[, as.character(weights)])

model1 <- svyglm(formula, design = des1)
model2 <- svyglm(formula, design = des2)

means1 <- weighted_means(des1, "y")
means2 <- weighted_means(des2, "y")

"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" %>% print()
c(means1 = means1, means2 = means2, means_dif = (means1 - means2)) %>% print()
