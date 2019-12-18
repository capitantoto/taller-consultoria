df2
df2_ <- df2 %>%
  mutate(gm = mean(rend)) %>%
  group_by(var) %>% mutate(varm = mean(rend) - gm) %>% ungroup() %>%
  group_by(loc) %>% mutate(locm = mean(rend) - gm) %>% ungroup() %>%
  mutate(lambda = varm*locm)

lm2_ <- lm(rend ~ loc + var + lambda, df2_)

dae::tukey.1df(aov(lm2), df2)

augment(lm2_, df2_) %>%
  ggplot(aes(.fitted, .std.resid)) +
  geom_point() +
  labs(title = "Residuos en función de rindes predichos para un modelo aditivo",
       x = "Rinde predicho", y = "Residuo")

lm2 <- lm(rend ~ loc + var, df2)

augment(lm2, df2) %>%
  ggplot(aes(.fitted, .std.resid)) +
  geom_point() +
  labs(title = "Residuos en función de rindes predichos para un modelo aditivo",
       x = "Rinde predicho", y = "Residuo")

cross <- augment(lm2, df2_)

lm(.resid ~ lambda, cross) %>% summary
  plot(lm2_)
map(lst(lm2, lm2_), anova)
map(lst(lm2, lm2_), summary)

augment(lm2_, df2_) %>%
  as_tibble() %>%
  select(loc, var, rend, .fitted) %>%
  gather("tipo", "rend", -loc, -var) %>%
  ggplot(aes(loc, rend, color = var, shape = tipo)) +
  geom_point() +
  facet_wrap(~var)

lambdahat <- lm2_$coefficients["lambda"]
df2_ %>%
  transmute(loc, crit = 1 + lambdahat*locm) %>%
  distinct()

locs <- levels(df2$loc)
nlocs <- length(locs)
cortes <- list()
trCont <- trainControl(method = "repeatedcv", number = 10, repeats = 40)
f <- rend ~ (loc + var) * low
obs <- df2$rend
for (i in seq_along(locs)) {
  low_locs <- locs[1:i]
  df2_cut <- mutate(df2, low = loc %in% low_locs)
  lmObj <- lm(f, df2_cut)
  pred <- predict(lmObj)
  trainObj <- train(f, df2_cut, method = "lm", trControl = trCont)
  res <- list(
    low = paste(low_locs, collapse = ", "),
    lmObj = lmObj,
    trainObj = trainObj,
    rmse_train = caret::RMSE(pred, obs),
    rmse_test = trainObj$results$RMSE,
    rmse_test_sd = trainObj$results$RMSESD
  )
  cortes <- c(cortes, list(res))
}

res <- transpose(cortes) %>%
  as_tibble() %>%
  mutate_at(c("rmse_test", "rmse_test_sd", "low", "rmse_train"), unlist) %>%
  select(-lmObj, -trainObj) %>%
  View()

best_low <- c(12, 3, 11, 4, 9, 6)
df2 <- df2 %>%
  mutate(low = loc %in% best_low)

df2_low <- filter(df2, loc %in% best_low)
df2_high <- filter(df2, !loc %in% best_low)

lm2_all <- lm(rend ~ loc + var * low, df2)
lm2_low <- lm(rend ~ loc + var, df2_low)
lm2_high <- lm(rend ~ loc + var, df2_high)
map(lst(lm2_all, lm2_low, lm2_high), summary)
plot(lm2_low)
plot(lm2)
augment(lm2_low, df2)
TukeyHSD(aov(lm2_low))
TukeyHSD(aov(lm2_high))
