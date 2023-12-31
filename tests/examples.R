# Example usage of OaxacaSurvey

# Load required packages

library(OaxacaSurvey) # latest version of this package

library(data.table) # optional, for data import and handling
library(magrittr) # optional, for piping with %>%


############################ Test code ############################

# Import dataset and prepare it for SurveyOaxaca
df <- fread("tests/eff-pool-2002-2020.csv")
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
  R = 10,
  method = "normal"
)

# Return Oaxaca-Blinder decomposition with bootestraped CI
result %>% print()

# plot the Oaxaca-Blinder decomposition in bars
result["value", ][1:4] %>% as.matrix() %>% barplot()