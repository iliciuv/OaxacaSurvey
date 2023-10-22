### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
`%>%` <- magrittr::`%>%` # nolint
options(scipen = 99)
rif_var <- "quantile"
c("survey", "data.table", "boot", "dineq") %>% sapply(library, character.only = TRUE)


oaxaca_blinder_svy <- function(formula, data, group, weights, R = 1000, conf.level = 0.95) {
  # Oaxaca-Blinder Decomposition on a single dataset

  single_decomposition <- function(data, indices) {
    data <- data[indices, ] # obtain the bootstrapped sample
    return(oaxaca_blinder_core(data, formula, group, weights))
  }

  weighted_means <- function(design, variables) {
    means <- sapply(variables, function(v) {
      as.numeric(survey::svymean(as.formula(paste0("~ ", v)), design = design))
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
    des1 <- survey::svydesign(ids = ~1, data = data1, weights = data1[, as.character(weights)])
    des2 <- survey::svydesign(ids = ~1, data = data2, weights = data2[, as.character(weights)])

    # Estimate svygml model accounting for survey design
    model1 <- survey::svyglm(formula, design = des1)
    model2 <- survey::svyglm(formula, design = des2)

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
  boot.result <- boot::boot(data = data, statistic = single_decomposition, R = R)

  # Compute Confidence Intervals
  alpha <- 1 - conf.level
  ci.lower <- apply(boot.result$t, 2, function(x) quantile(x, alpha / 2, na.rm = TRUE))
  ci.upper <- apply(boot.result$t, 2, function(x) quantile(x, 1 - alpha / 2, na.rm = TRUE))

  # Return results as a list
  result <- data.frame(
    unex = c(value = mean(boot.result$t[, 1]), CI = c(ci.lower[1], ci.upper[1])),
    end = c(value = mean(boot.result$t[, 2]), CI = c(ci.lower[2], ci.upper[2])),
    coef = c(value = mean(boot.result$t[, 3]), CI = c(ci.lower[3], ci.upper[3])),
    inter = c(value = mean(boot.result$t[, 4]), CI = c(ci.lower[4], ci.upper[4])),
    total = c(value = mean(boot.result$t[, 5]), CI = c(ci.lower[5], ci.upper[5])),
    means1_y = c(value = mean(boot.result$t[, 6]), CI = c(ci.lower[6], ci.upper[6])),
    means2_y = c(value = mean(boot.result$t[, 7]), CI = c(ci.lower[7], ci.upper[7])),
    means_dif = c(value = mean(boot.result$t[, 8]), CI = c(ci.lower[8], ci.upper[8]))
  )
  return(result)
}


# Import dataset and perform data manipulation
df <- fread("tests/eff-pool-2002-2020.csv")
df[, group := 0][class == "worker", group := 0][class == "capitalist", group := 1]
df[, rentsbi := 0][rents >= renthog * 0.1 & rents > 2000, rentsbi := 1]
df[homeowner == "", homeowner := "Non-Owner"]
df$class <- relevel(as.factor(df$class), ref = "self-employed")
df$bage <- relevel(as.factor(df$bage), ref = "0-34")
df$inherit <- relevel(as.factor(df$inherit), ref = "Non-inherit")
df$homeowner <- relevel(as.factor(df$homeowner), ref = "Non-Owner")
df$riquezafin <- factor(as.logical(df$riquezafin), levels = c(T, F), labels = c("Fin", "NonFin"))
total_variables <- c(
  "facine3", "renthog", "renthog1", "bage", "homeowner", "worker", "young", "sex", "class",
  "actreales", "riquezanet", "riquezafin", "educ", "auton", "class",
  "tipo_auton", "direc", "multipr", "useprop", "inherit"
)

formula <- rentsbi ~ bage + sex + educ + riquezafin + inherit + direc + homeowner + multipr
df <- df[sv_year == 2020]


############### Test lm (with and without weights) vs svyglm #############

# Define survey design accounting for sample weights and other characteristics
df_sv <- survey::svydesign(ids = ~1, data = data.frame(df), weights = df$facine3)

# Estimate svygml model accounting for survey design
model1 <- survey::svyglm(formula, design = df_sv)
model2 <- lm(formula, weights = facine3, data = df)
model3 <- lm(formula, data = df)

model1 %>%
  coef() %>%
  head()
model2 %>%
  coef() %>%
  head()
model3 %>%
  coef() %>%
  head()

# Results:
# (Intercept)   bage35-44   bage45-54   bage54-65   bage65-75      bage75
#   24189.150  -15692.831  -13640.406   -9968.111  -10328.491   -5551.017
# (Intercept)   bage35-44   bage45-54   bage54-65   bage65-75      bage75
#   24189.150  -15692.831  -13640.406   -9968.111  -10328.491   -5551.017
# (Intercept)   bage35-44   bage45-54   bage54-65   bage65-75      bage75
#   21912.893  -11396.627  -11450.651   -7090.570   -6794.408   -6662.884

############################ Test code ############################

# Apply "oaxaca_blinder_svy" function to simulated data
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
selected_variables <- c(
  "renthog", "riquezanet", "bage", "sex", "educ", "renthog1", "inherit", "homeowner", "riquezafin"
)

# adaption to OaxacaSurvey function
data <- df[, ..selected_variables]
data <- fastDummies::dummy_cols(data,
  select_columns = c("bage", "sex", "educ", "renthog1", "inherit", "homeowner", "riquezafin"),
  remove_first_dummy = FALSE,
  remove_selected_columns = TRUE
)
colnames(data) <- paste0("x", seq_along(colnames(data)))
length_reg <- length(colnames(data))
new_formula <- paste("y ~", paste0("x", 1:length_reg, collapse = " + "))
data <- cbind(y = df$rentsbi, group = df$group, weights = df$facine3, data)



result2 <- oaxaca_blinder_svy(
  as.formula(new_formula),
  data = data.frame(data),
  group = "group",
  weights = "weights",
  R = 10
)
print(result2)