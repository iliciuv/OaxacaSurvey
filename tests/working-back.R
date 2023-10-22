library(survey)
library(boot)
library(data.table)
library(magrittr)
library(OaxacaSurvey
)
####################################################################################

df <- fread("tests/eff-pool-2002-2020.csv")
df <- df[sv_year == 2020]
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
selected_variables <- c(
  "riquezanet", "bage", "sex", "educ", "inherit", "homeowner", "riquezafin"
)

# adaption to OaxacaSurvey function
data <- df[, ..selected_variables]
data <- fastDummies::dummy_cols(data,
  select_columns = c("bage", "sex", "educ", "inherit", "homeowner", "riquezafin"),
  remove_first_dummy = FALSE,
  remove_selected_columns = TRUE
)
colnames(data) <- paste0("x", seq_along(colnames(data)))
length_reg <- length(colnames(data))
new_formula <- paste("y ~", paste0("x", 1:length_reg, collapse = " + "))
data <- cbind(y = df$renthog, group = df$group, weights = df$facine3, data)



result2 <- oaxaca_blinder_svy(
  as.formula(new_formula),
  data = data.frame(data),
  group = "group",
  weights = "weights",
  R = 10
)
print(result2)