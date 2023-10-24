# Simulated data
library(data.table)
library(oaxaca)
library(dineq)
library(survey)

df <- fread("tests/eff-pool-2002-2020.csv")
df <- df[sv_year %in% c(2020)][, group := 0][class == "worker", group := 1]
cpi <- c(73.31, 80.44, 89.11, 93.35, 96.82, 97.98, 100) / 100
years <- c(2002, 2005, 2008, 2011, 2014, 2017, 2020)
df[, rentsbi := 0][rents >= renthog * 0.1 & rents > 2000, rentsbi := 1][]
df[homeowner == "", homeowner := "Non-Owner"]
df$class <- relevel(as.factor(df$class), ref = "self-employed")
df$bage <- relevel(as.factor(df$bage), ref = "0-34")
df$inherit <- relevel(as.factor(df$inherit), ref = "Non-inherit")
df$homeowner <- relevel(as.factor(df$homeowner), ref = "Non-Owner")
df$riquezafin <- factor(as.logical(df$riquezafin), levels = c(T, F), labels = c("Fin", "NonFin"))
df <- data.frame(df)
sv_df <- svydesign(ids = ~1, data = df, weights = df$facine3)
set.seed(123)

# Perform Oaxaca decomposition, passing weights to lm
result1 <- oaxaca(
  formula = rentsbi ~ bage + sex + riquezafin + actreales | group,
  data = df,
  R = 1
)


result2 <- oaxaca(
  formula = rentsbi ~ bage + sex + riquezafin + actreales | group,
  data = df,
  reg.fun = function(formula, data, ...) {
    glm(formula, data = data, family =  quasibinomial())
  },
    R = 1
)


##############

result3 <- oaxaca(
  formula = rentsbi ~ bage + sex + riquezafin + actreales | group,
  data = df,
  R = 1,
  reg.fun = function(formula, data, ...) {
    lm(formula, data = data,  weights  =  facine3)
  }
)

#print(all.equal(result1, result2))
#print(all.equal(result1, result3))

print(result1$threefold$overall)
print(result2$threefold$overall)
print(result3$threefold$overall)
print(result1$y)
print(result2$y)
print(result3$y)
