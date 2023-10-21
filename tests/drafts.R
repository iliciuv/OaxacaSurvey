


### WORKSPACE SETUP- MEMORY CLEAN AND PACKAGES IMPORT
`%>%` <- magrittr::`%>%` # nolint
options(scipen = 99)
rif_var <- "quantile"
c("survey", "data.table", "boot", "dineq", "OaxacaSurvey") %>% sapply(library, character.only = T)
selected_variables <- c(
    "facine3", "renthog", "renthog1", "bage", "homeowner", "worker", "young", "sex", "class",
    "actreales", "riquezanet", "riquezafin", "educ", "auton", "class",
    "tipo_auton", "direc", "multipr", "useprop", "inherit"
)
dt_eff <- "tests/eff-pool-2002-2020.csv" %>% fread() # Data table con microdatos anuales
# Convert 'class' and 'bage' to dummy variables
dt_eff[, ..selected_variables]
dt_eff[is.na(dt_eff)] <- 0
final_dt <- data.table()
models_dt <- list()

cpi <- c(73.31, 80.44, 89.11, 93.35, 96.82, 97.98, 100) / 100
years <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020)
dt_eff[homeowner == "", homeowner := "Non-Owner"]
dt_eff$class <- relevel(as.factor(dt_eff$class), ref = "self-employed")
dt_eff$bage <- relevel(as.factor(dt_eff$bage), ref = "0-34")
dt_eff$inherit <- relevel(as.factor(dt_eff$inherit), ref = "Non-inherit")
dt_eff$homeowner <- relevel(as.factor(dt_eff$homeowner), ref = "Non-Owner")
dt_eff$riquezafin <- factor(as.logical(dt_eff$riquezafin), levels = c(T, F), labels = c("Fin", "NonFin"))

# dt_eff <- fastDummies::dummy_cols(dt_eff, select_columns = c("class"))
for (i in seq_along(years)) {
    dt_transform <- dt_eff[sv_year == years[i]]
    # Estimate RIF model
    dt_transform$rif_rents <- rif(dt_transform$rents, method = as.character(rif_var), quantile = 0.5)
    models_dt[[i]] <- lm(rif_rents ~ bage + sex + educ + riquezafin + inherit + direc + homeowner + multipr, weights = facine3, data = dt_transform)
}


# Import dataset and prepare it for SurveyOaxaca
df <- fread("tests/eff-pool-2002-2020.csv")
df[sv_year == 2020]
df[, group := 0][class == "worker", group := 0][class == "capitalist", group := 1]
df[, rentsbi := 0][rents >= renthog * 0.1 & rents > 2000, rentsbi := 1]
# Define data object simulating a suvey with sampling weigths (variable w)
data <- data.frame(
  y = df$renthog,
  x1 = df$rentsbi,
  x2 = as.numeric(as.factor(df$homeowner)) - 2,
  group = df$group,
  weights = df$facine3
)

# Apply "oaxaca_blinder_svy" function to simulated data
result <- oaxaca_blinder_svy(
  y ~ x1 + x2,
  data = data,
  group = "group",
  weights = "weights",
  R = 10
)

# Return Oaxaca-Blinder decomposition with bootestraped CI

result %>% print()



############################ Test code ############################



#  if data frame
# data <- df[, selected_variables, drop = FALSE]
data <- df[, ..selected_variables]
# if names
# names(data) <- paste0("x", seq_along(selected_variables))
colnames(data) <- paste0("x", seq_along(selected_variables))


lm(rif_rents ~ bage + sex + educ + riquezafin + inherit + direc + homeowner + multipr, weights = facine3, data = dt_transform)

# Split 2 distinct control groups
data1 <- data[data$group == 1, ]

# Define survey design accounting for sample weights and other characteristics
des1 <- survey::svydesign(ids = ~1, data = data1, weights = data1[,as.character(weights)])

# Estimate svygml model accounting for survey design
model1 <- survey::svyglm(formula, design = des1)