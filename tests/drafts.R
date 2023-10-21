### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
`%>%` <- magrittr::`%>%` # nolint
options(scipen = 99)
rif_var <- "quantile"
c("survey", "data.table", "boot", "dineq", "OaxacaSurvey") %>% sapply(library, character.only = T)

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
df[, rif_rents := rif(rents)]
total_variables <- c(
    "facine3", "renthog", "renthog1", "bage", "homeowner", "worker", "young", "sex", "class",
    "actreales", "riquezanet", "riquezafin", "educ", "auton", "class",
    "tipo_auton", "direc", "multipr", "useprop", "inherit"
)
selected_variables <- c(
    "facine3", "renthog1", "bage", "homeowner", "sex", "class",
   "riquezanet",  "educ", "auton","direc", "multipr", "inherit"
)
df <- df[sv_year == 2020]


############### Test lm (with and without weights) vs svyglm #############

# Define survey design accounting for sample weights and other characteristics
df_sv <- survey::svydesign(ids = ~1, data = data.frame(df), weights = df$facine3)

# Estimate svygml model accounting for survey design
model1 <- survey::svyglm(formula, design = df_sv)
model2 <- lm(formula, weights = facine3, data = df)
model3 <- lm(formula, data = df)

model1 %>% coef() %>% head() %>% print
model2 %>% coef() %>% head() %>% print
model3 %>% coef() %>% head() %>% print

# Results:
# (Intercept)   bage35-44   bage45-54   bage54-65   bage65-75      bage75
#   24189.150  -15692.831  -13640.406   -9968.111  -10328.491   -5551.017
# (Intercept)   bage35-44   bage45-54   bage54-65   bage65-75      bage75
#   24189.150  -15692.831  -13640.406   -9968.111  -10328.491   -5551.017
# (Intercept)   bage35-44   bage45-54   bage54-65   bage65-75      bage75
#   21912.893  -11396.627  -11450.651   -7090.570   -6794.408   -6662.884




############################ Test code ############################

# adaption to OaxacaSurvey function
data <- df[, ..selected_variables]
colnames(data) <- paste0("x", seq_along(selected_variables))
formula <- rif_rents ~ bage + sex + educ + riquezafin + inherit + direc + homeowner + multipr

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
