library(readxl)
library(caret)
library(bnlearn)

### B1 ###

df <- read_excel("~/Desktop/Long_Distance_Carrier_Data.xls")
df <- as.data.frame(df)
df$X1 <- factor(df$X1, levels=c(0, 1), labels=c('low', 'high'))
df$X2 <- factor(df$X2, levels=c(0, 1), labels=c('low', 'high'))
df$X3 <- factor(df$X3, levels=c(0, 1), labels=c('low', 'high'))
df$Y <- factor(df$Y, levels=c(1, 2, 3), labels=c('A', 'B', 'C'))

ftable(df)
table(df$X1, df$Y)
table(df$X2, df$Y)
table(df$X3, df$Y)

### B2.1 ###

dag_mod <- model2network("[Y][X1|Y][X2|Y][X3|Y]")
plot(dag_mod)

### B2.2 ###

ftable(df$Y)

### B2.3 ###

ftable(df$Y) / sum(ftable(df$Y))
t(table(df$X1, df$Y)) / colSums(table(df$X1, df$Y))
t(table(df$X2, df$Y)) / colSums(table(df$X2, df$Y))
t(table(df$X3, df$Y)) / colSums(table(df$X3, df$Y))

### B2.4

mod1 <- naive.bayes(df, "Y")
mod1

mod2 <- bn.fit(
  dag_mod,
  data = df,
  method = "bayes",
  iss = 12
)
mod2

new_customer <- data.frame(
  X1 = factor("high", levels = levels(df$X1)),
  X2 = factor("low", levels = levels(df$X2)),
  X3 = factor("high", levels = levels(df$X3))
)
predict(mod2, node = "Y", data = new_customer, prob = TRUE)

cpquery(
  mod2,
  event = (Y == "A"),
  evidence = (X1 == "high" & X2 == "low" & X3 == "high")
)
cpquery(
  mod2,
  event = (Y == "B"),
  evidence = (X1 == "high" & X2 == "low" & X3 == "high")
)
cpquery(
  mod2,
  event = (Y == "C"),
  evidence = (X1 == "high" & X2 == "low" & X3 == "high")
)

### B3 ###

fit_1 <- bn.fit(mod1, df, method = "bayes", iss = 1)
predict(fit_1, data = new_customer, prob = TRUE)

fit_12 <- bn.fit(mod1, df, method = "bayes", iss = 12)
predict(fit_12, data = new_customer, prob = TRUE)

fit_100 <- bn.fit(mod1, df, method = "bayes", iss = 100)
predict(fit_100, data = new_customer, prob = TRUE)

fit_1000 <- bn.fit(mod1, df, method = "bayes", iss = 1000)
predict(fit_1000, data = new_customer, prob = TRUE)

### B4.2.a ###

dag_m1 <- model2network("[Y][X1|Y][X2|Y:X1][X3|Y:X2:X1]")
plot(dag_m1)
fit_m1 <- bn.fit(dag_m1, df, method = "bayes", iss = 12)
       
dag_m2 <- model2network("[Y][X1|Y][X2|Y][X3|Y]")
plot(dag_m2)
fit_m2 <- bn.fit(dag_m2, df, method = "bayes", iss = 12)

dag_m3 <- model2network("[Y][X1|Y][X2|Y:X1][X3|Y]")
plot(dag_m3)
fit_m3 <- bn.fit(dag_m3, df, method = "bayes", iss = 12)

dag_m4 <- model2network("[Y][X1|Y][X2|Y][X3|Y:X1]")
plot(dag_m4)
fit_m4 <- bn.fit(dag_m4, df, method = "bayes", iss = 12)

dag_m5 <- model2network("[Y][X1|Y][X2|Y][X3|Y:X2]")
plot(dag_m5)
fit_m5 <- bn.fit(dag_m5, df, method = "bayes", iss = 12)
fit_m5

ftable(df[, c('Y', 'X2','X3')])

### B4.2.b ###

profiles <- expand.grid(
  X1 = levels(df$X1),
  X2 = levels(df$X2),
  X3 = levels(df$X3)
)

# method=parents
pred_m1_p <- predict(fit_m1, node='Y', data = profiles, prob = TRUE, method='parents')
pred_m2_p <- predict(fit_m2, node='Y', data = profiles, prob = TRUE, method='parents')
pred_m3_p <- predict(fit_m3, node='Y', data = profiles, prob = TRUE, method='parents')
pred_m4_p <- predict(fit_m4, node='Y', data = profiles, prob = TRUE, method='parents')
pred_m5_p <- predict(fit_m5, node='Y', data = profiles, prob = TRUE, method='parents')

# method=bayes-lw
pred_m1_b <- predict(fit_m1, node='Y', data = profiles, prob = TRUE, method='bayes-lw')
pred_m2_b <- predict(fit_m2, node='Y', data = profiles, prob = TRUE, method='bayes-lw')
pred_m3_b <- predict(fit_m3, node='Y', data = profiles, prob = TRUE, method='bayes-lw')
pred_m4_b <- predict(fit_m4, node='Y', data = profiles, prob = TRUE, method='bayes-lw')
pred_m5_b <- predict(fit_m5, node='Y', data = profiles, prob = TRUE, method='bayes-lw')

# method=exact
pred_m1_e <- predict(fit_m1, node='Y', data = profiles, prob = TRUE, method='exact')
pred_m2_e <- predict(fit_m2, node='Y', data = profiles, prob = TRUE, method='exact')
pred_m3_e <- predict(fit_m3, node='Y', data = profiles, prob = TRUE, method='exact')
pred_m4_e <- predict(fit_m4, node='Y', data = profiles, prob = TRUE, method='exact')
pred_m5_e <- predict(fit_m5, node='Y', data = profiles, prob = TRUE, method='exact')

### B4.2.c ###

pred_m1 <- predict(fit_m1, node='Y', data = df, prob = TRUE, method='exact')
pred_m2 <- predict(fit_m2, node='Y', data = df, prob = TRUE, method='exact')
pred_m3 <- predict(fit_m3, node='Y', data = df, prob = TRUE, method='exact')
pred_m4 <- predict(fit_m4, node='Y', data = df, prob = TRUE, method='exact')
pred_m5 <- predict(fit_m5, node='Y', data = df, prob = TRUE, method='exact')

class_m1 <- rownames(attr(pred_m1, "prob"))[max.col(t(attr(pred_m1, "prob")))]
class_m2 <- rownames(attr(pred_m2, "prob"))[max.col(t(attr(pred_m2, "prob")))]
class_m3 <- rownames(attr(pred_m3, "prob"))[max.col(t(attr(pred_m3, "prob")))]
class_m4 <- rownames(attr(pred_m4, "prob"))[max.col(t(attr(pred_m4, "prob")))]
class_m5 <- rownames(attr(pred_m5, "prob"))[max.col(t(attr(pred_m5, "prob")))]

confusionMatrix(factor(class_m1, levels = levels(df$Y)), df$Y)
confusionMatrix(factor(class_m2, levels = levels(df$Y)), df$Y)
confusionMatrix(factor(class_m3, levels = levels(df$Y)), df$Y)
confusionMatrix(factor(class_m4, levels = levels(df$Y)), df$Y)
confusionMatrix(factor(class_m5, levels = levels(df$Y)), df$Y)

### B4.3 ###

score(dag_m1, data = df, type = "bde", iss = 12)
score(dag_m2, data = df, type = "bde", iss = 12)
score(dag_m3, data = df, type = "bde", iss = 12)
score(dag_m4, data = df, type = "bde", iss = 12)
score(dag_m5, data = df, type = "bde", iss = 12)

exp(score(dag_m1, data = df, type = "bde", iss = 12) + log(1/5))
exp(score(dag_m2, data = df, type = "bde", iss = 12) + log(1/5))
exp(score(dag_m3, data = df, type = "bde", iss = 12) + log(1/5))
exp(score(dag_m4, data = df, type = "bde", iss = 12) + log(1/5))
exp(score(dag_m5, data = df, type = "bde", iss = 12) + log(1/5))



score(dag_m1, data = df, type = "bde", iss = 12) + log(1/5)
score(dag_m2, data = df, type = "bde", iss = 12) + log(1/5)
score(dag_m3, data = df, type = "bde", iss = 12) + log(1/5)
score(dag_m4, data = df, type = "bde", iss = 12) + log(1/5)
score(dag_m5, data = df, type = "bde", iss = 12) + log(1/5)





