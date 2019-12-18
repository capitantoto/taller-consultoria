library(tidyverse)
library(broom)
library(caret)
vol <- 3 # ml/ petri
df <- read_csv("data/5-1.csv")
titas <- df %>%
  mutate(dilucion = dilucion - 1) %>%
  group_by(muestra, dilucion) %>%
  summarise(ufp_ = mean(ufp)) %>%
  mutate(fct_dil = (1/3)^dilucion) %>%
  summarise(
    tita = sum(ufp_) / (vol * sum(fct_dil)))
  
merge(df, titas, by = "muestra") %>%
  as_tibble() %>%
  mutate(
    ml_orig = vol * (1/3)^(dilucion-1),
    ufp_ml_orig = ufp / ml_orig)
  
titas
df <- df %>%
  mutate_at(c("muestra", "dilucion", "replica"), as_factor) %>%
  mutate_at("ufp", as.integer)

modelos_lin <- list(
  lin_add = ufp ~ muestra + dilucion,
  lin_add2 = ufp ~ muestra,
  lin_mul = ufp ~ muestra * dilucion
)
modelos_sqrt <- list(
  sqrt_add = sqrt(ufp) ~ muestra + dilucion,
  sqrt_mul = sqrt(ufp) ~ muestra * dilucion
)

modelos <- c(modelos_lin, modelos_sqrt)
trCont_lin <- trainControl(method = "repeatedcv", number = 5, repeats = 40,
                           summaryFunction = adHocSummary(inversa = identity))
trCont_sqrt <- trainControl(method = "repeatedcv", number = 5, repeats = 40,
                           summaryFunction = adHocSummary(inversa = function(x) x^2))
res <- bind_rows(
  comparar(modelos_lin, df, trCont_lin),
  comparar(modelos_sqrt, df, trCont_sqrt)
)

map(modelos, ~anova(lm(., df)))

map(lst(lin_add, lin_mul, sqrt_add, sqrt_mul), summary)

lin_mul$residuals
map(lst(lin_add, lin_mul, sqrt_add, sqrt_mul), summary)
caret::RMSE(predict(sqrt_mul$model), sqrt(df$ufp))
