# ============================================================
# DNSC 8328 - Assignment 3
# Part B: Bayesian Network Analysis
# ============================================================


# ============================================================
# 0. Setup
# ============================================================

# Run only once if packages are not installed:
# install.packages("bnlearn")
# install.packages("gRain")

library(bnlearn)
library(gRain)

# Set seed for all simulation-based procedures
set.seed(8289)


# ============================================================
# 1. Read and prepare the data
# ============================================================

dat <- read.csv("carrier_data.csv")

# Recode all variables as factors
dat$Y <- factor(
  dat$Y,
  levels = c(1, 2, 3),
  labels = c("A", "B", "C")
)

dat$X1 <- factor(
  dat$X1,
  levels = c(0, 1),
  labels = c("low", "high")
)

dat$X2 <- factor(
  dat$X2,
  levels = c(0, 1),
  labels = c("low", "high")
)

dat$X3 <- factor(
  dat$X3,
  levels = c(0, 1),
  labels = c("low", "high")
)

# Check the data
str(dat)
head(dat)
summary(dat)

# Sample size
nrow(dat)



# ============================================================
# B1. Exploratory contingency tables
# ============================================================

# ------------------------------------------------------------
# B1(a) Four-way contingency table
# ------------------------------------------------------------

tab4 <- table(
  Y = dat$Y,
  X1 = dat$X1,
  X2 = dat$X2,
  X3 = dat$X3
)

tab4

# More readable version
ftable(tab4)

# Data-frame version if needed for report
as.data.frame(tab4)


# ------------------------------------------------------------
# B1(b) Cross-tabulations of Y with X1, X2, X3
# ------------------------------------------------------------

tab_Y_X1 <- table(
  Y = dat$Y,
  X1 = dat$X1
)

tab_Y_X2 <- table(
  Y = dat$Y,
  X2 = dat$X2
)

tab_Y_X3 <- table(
  Y = dat$Y,
  X3 = dat$X3
)

tab_Y_X1
tab_Y_X2
tab_Y_X3


# Row proportions:
# distribution of satisfaction level within each provider

prop_Y_X1 <- prop.table(tab_Y_X1, margin = 1)
prop_Y_X2 <- prop.table(tab_Y_X2, margin = 1)
prop_Y_X3 <- prop.table(tab_Y_X3, margin = 1)

round(prop_Y_X1, 4)
round(prop_Y_X2, 4)
round(prop_Y_X3, 4)


# Optional: column proportions
round(prop.table(tab_Y_X1, margin = 2), 4)
round(prop.table(tab_Y_X2, margin = 2), 4)
round(prop.table(tab_Y_X3, margin = 2), 4)



# ============================================================
# B2. Naive Bayes Model
# ============================================================


# ============================================================
# B2(a) Construct naive Bayes DAG
# ============================================================

# Automatically construct naive Bayes network
nb <- naive.bayes(
  dat,
  training = "Y"
)

nb

# Display model string
modelstring(nb)

# Check arcs
arcs(nb)

# DAG:
# Y -> X1
# Y -> X2
# Y -> X3
#
# Factorization:
# p(Y, X1, X2, X3)
# = p(Y) p(X1 | Y) p(X2 | Y) p(X3 | Y)



# ============================================================
# B2(b) Bayesian parameter estimation with alpha = 12
# ============================================================

fit_nb <- bn.fit(
  nb,
  data = dat,
  method = "bayes",
  iss = 12
)

fit_nb


# Posterior mean CPTs
fit_nb$Y
fit_nb$X1
fit_nb$X2
fit_nb$X3



# ============================================================
# B2(c) Counts and Dirichlet posterior parameters
# ============================================================

# ------------------------------------------------------------
# Y
# ------------------------------------------------------------

table(dat$Y)

# Prior for Y:
# alpha = 12
# C_Y = 3, Q_Y = 1
#
# alpha_ijk = 12 / (3 * 1) = 4
#
# prior:
# Dirichlet(4, 4, 4)
#
# observed counts:
# A = 333
# B = 380
# C = 164
#
# posterior:
# Dirichlet(337, 384, 168)


# ------------------------------------------------------------
# X1 | Y
# ------------------------------------------------------------

table(
  X1 = dat$X1,
  Y = dat$Y
)

# For each Y configuration:
# C_X1 = 2
# Q_X1 = 3
#
# alpha_ijk = 12 / (2 * 3) = 2
#
# prior:
# Dirichlet(2, 2)

# Posterior:
# Y=A: Dirichlet(205, 132)
# Y=B: Dirichlet(135, 249)
# Y=C: Dirichlet(114, 54)


# ------------------------------------------------------------
# X2 | Y
# ------------------------------------------------------------

table(
  X2 = dat$X2,
  Y = dat$Y
)

# Posterior:
# Y=A: Dirichlet(198, 139)
# Y=B: Dirichlet(244, 140)
# Y=C: Dirichlet(106, 62)


# ------------------------------------------------------------
# X3 | Y
# ------------------------------------------------------------

table(
  X3 = dat$X3,
  Y = dat$Y
)

# Posterior:
# Y=A: Dirichlet(163, 174)
# Y=B: Dirichlet(176, 208)
# Y=C: Dirichlet(93, 75)



# ============================================================
# B2(d) Hand classification for:
# X1 = high, X2 = low, X3 = high
# ============================================================

# Use posterior mean probabilities from fitted BN

# Class A
score_A <-
  fit_nb$Y$prob["A"] *
  fit_nb$X1$prob["high", "A"] *
  fit_nb$X2$prob["low", "A"] *
  fit_nb$X3$prob["high", "A"]

# Class B
score_B <-
  fit_nb$Y$prob["B"] *
  fit_nb$X1$prob["high", "B"] *
  fit_nb$X2$prob["low", "B"] *
  fit_nb$X3$prob["high", "B"]

# Class C
score_C <-
  fit_nb$Y$prob["C"] *
  fit_nb$X1$prob["high", "C"] *
  fit_nb$X2$prob["low", "C"] *
  fit_nb$X3$prob["high", "C"]

scores <- c(
  A = score_A,
  B = score_B,
  C = score_C
)

scores

# Normalize
hand_probs <- scores / sum(scores)

hand_probs

# Predicted class
names(which.max(hand_probs))



# ============================================================
# B2(e) Reproduce using predict()
# ============================================================

new_customer <- data.frame(
  X1 = factor(
    "high",
    levels = levels(dat$X1)
  ),
  X2 = factor(
    "low",
    levels = levels(dat$X2)
  ),
  X3 = factor(
    "high",
    levels = levels(dat$X3)
  )
)

new_customer


pred_nb_exact <- bnlearn:::predict.bn.fit(
  fit_nb,
  node = "Y",
  data = new_customer,
  prob = TRUE,
  method = "exact"
)

pred_nb_exact

attr(
  pred_nb_exact,
  "prob"
)



# ============================================================
# B2(f) Reproduce using cpquery()
# ============================================================

# cpquery() uses simulation, so set a seed

set.seed(8289)

pA_cp <- cpquery(
  fit_nb,
  event = (Y == "A"),
  evidence = (
    X1 == "high" &
      X2 == "low" &
      X3 == "high"
  ),
  n = 100000
)

set.seed(8289)

pB_cp <- cpquery(
  fit_nb,
  event = (Y == "B"),
  evidence = (
    X1 == "high" &
      X2 == "low" &
      X3 == "high"
  ),
  n = 100000
)

set.seed(8289)

pC_cp <- cpquery(
  fit_nb,
  event = (Y == "C"),
  evidence = (
    X1 == "high" &
      X2 == "low" &
      X3 == "high"
  ),
  n = 100000
)

cp_probs <- c(
  A = pA_cp,
  B = pB_cp,
  C = pC_cp
)

cp_probs

# The sum may not be exactly 1 because
# each cpquery() call uses Monte Carlo simulation
sum(cp_probs)



# ============================================================
# B3. Sensitivity to global precision alpha
# ============================================================

alphas <- c(
  1,
  12,
  100,
  1000
)

alpha_results <- matrix(
  NA,
  nrow = length(alphas),
  ncol = 3
)

rownames(alpha_results) <- alphas
colnames(alpha_results) <- c(
  "A",
  "B",
  "C"
)


for (i in seq_along(alphas)) {
  
  alpha_i <- alphas[i]
  
  fit_i <- bn.fit(
    nb,
    data = dat,
    method = "bayes",
    iss = alpha_i
  )
  
  pred_i <- bnlearn:::predict.bn.fit(
    fit_i,
    node = "Y",
    data = new_customer,
    prob = TRUE,
    method = "exact"
  )
  
  alpha_results[i, ] <-
    attr(pred_i, "prob")[, 1]
}


alpha_results

round(
  alpha_results,
  4
)


# Create table for report

alpha_table <- data.frame(
  alpha = alphas,
  A = alpha_results[, "A"],
  B = alpha_results[, "B"],
  C = alpha_results[, "C"]
)

alpha_table



# ============================================================
# B4. Comparison of five Bayesian Network classifiers
# ============================================================


# ============================================================
# B4.1 Define the five DAGs
# ============================================================

# ------------------------------------------------------------
# M1: Full dependence among X1, X2, X3 given Y
# ------------------------------------------------------------

M1 <- model2network(
  "[Y][X1|Y][X2|Y:X1][X3|Y:X1:X2]"
)


# ------------------------------------------------------------
# M2: Naive Bayes
# ------------------------------------------------------------

M2 <- model2network(
  "[Y][X1|Y][X2|Y][X3|Y]"
)


# ------------------------------------------------------------
# M3:
# X3 independent of (X1, X2) given Y
# X1 and X2 allowed to depend
# ------------------------------------------------------------

M3 <- model2network(
  "[Y][X1|Y][X2|Y:X1][X3|Y]"
)


# ------------------------------------------------------------
# M4:
# X2 independent of (X1, X3) given Y
# X1 and X3 allowed to depend
# ------------------------------------------------------------

M4 <- model2network(
  "[Y][X1|Y][X2|Y][X3|Y:X1]"
)


# ------------------------------------------------------------
# M5:
# X1 independent of (X2, X3) given Y
# X2 and X3 allowed to depend
# ------------------------------------------------------------

M5 <- model2network(
  "[Y][X1|Y][X2|Y][X3|Y:X2]"
)


# Check model structures
modelstring(M1)
modelstring(M2)
modelstring(M3)
modelstring(M4)
modelstring(M5)

arcs(M1)
arcs(M2)
arcs(M3)
arcs(M4)
arcs(M5)



# ============================================================
# B4.1 Factorizations
# ============================================================

# M1:
# p(Y)
# p(X1 | Y)
# p(X2 | Y, X1)
# p(X3 | Y, X1, X2)

# M2:
# p(Y)
# p(X1 | Y)
# p(X2 | Y)
# p(X3 | Y)

# M3:
# p(Y)
# p(X1 | Y)
# p(X2 | Y, X1)
# p(X3 | Y)

# M4:
# p(Y)
# p(X1 | Y)
# p(X2 | Y)
# p(X3 | Y, X1)

# M5:
# p(Y)
# p(X1 | Y)
# p(X2 | Y)
# p(X3 | Y, X2)



# ============================================================
# B4.2(a) Fit all five models with alpha = 12
# ============================================================

fit_M1 <- bn.fit(
  M1,
  data = dat,
  method = "bayes",
  iss = 12
)

fit_M2 <- bn.fit(
  M2,
  data = dat,
  method = "bayes",
  iss = 12
)

fit_M3 <- bn.fit(
  M3,
  data = dat,
  method = "bayes",
  iss = 12
)

fit_M4 <- bn.fit(
  M4,
  data = dat,
  method = "bayes",
  iss = 12
)

fit_M5 <- bn.fit(
  M5,
  data = dat,
  method = "bayes",
  iss = 12
)



# ============================================================
# B4.2(a) Explicit posterior example using M3
# ============================================================

# M3 structure:
#
# Y -> X1
# Y -> X2
# X1 -> X2
# Y -> X3
#
# X2 has parents Y and X1.


# Counts for X2 conditional on Y and X1

table(
  X2 = dat$X2,
  Y = dat$Y,
  X1 = dat$X1
)


# Posterior mean CPT from bnlearn

fit_M3$X2


# Prior for X2 in M3:
#
# C_X2 = 2
# Q_X2 = 3 * 2 = 6
#
# alpha_ijk
# = 12 / (2 * 6)
# = 1
#
# Therefore prior for every parent configuration:
# Dirichlet(1, 1)
#
# Posterior parameters:
#
# Y=A, X1=low:
# Dirichlet(149, 56)
#
# Y=A, X1=high:
# Dirichlet(49, 83)
#
# Y=B, X1=low:
# Dirichlet(117, 18)
#
# Y=B, X1=high:
# Dirichlet(127, 122)
#
# Y=C, X1=low:
# Dirichlet(91, 23)
#
# Y=C, X1=high:
# Dirichlet(15, 39)



# ============================================================
# B4.2(b) Create all 8 possible attribute profiles
# ============================================================

profiles <- expand.grid(
  X1 = factor(
    c("low", "high"),
    levels = levels(dat$X1)
  ),
  X2 = factor(
    c("low", "high"),
    levels = levels(dat$X2)
  ),
  X3 = factor(
    c("low", "high"),
    levels = levels(dat$X3)
  )
)

profiles



# ============================================================
# B4.2(b) Compare parents, bayes-lw, and exact using M3
# ============================================================


# ------------------------------------------------------------
# parents
# ------------------------------------------------------------

pred_M3_parents <- bnlearn:::predict.bn.fit(
  fit_M3,
  node = "Y",
  data = profiles,
  prob = TRUE,
  method = "parents"
)

pred_M3_parents

attr(
  pred_M3_parents,
  "prob"
)


# ------------------------------------------------------------
# bayes-lw
# ------------------------------------------------------------

set.seed(8289)

pred_M3_lw <- bnlearn:::predict.bn.fit(
  fit_M3,
  node = "Y",
  data = profiles,
  prob = TRUE,
  method = "bayes-lw"
)

pred_M3_lw

attr(
  pred_M3_lw,
  "prob"
)


# ------------------------------------------------------------
# exact
# ------------------------------------------------------------

pred_M3_exact <- bnlearn:::predict.bn.fit(
  fit_M3,
  node = "Y",
  data = profiles,
  prob = TRUE,
  method = "exact"
)

pred_M3_exact

attr(
  pred_M3_exact,
  "prob"
)



# ============================================================
# B4.2(c) Exact predictions for all five models
# ============================================================

predict_exact <- function(fit, newdata) {
  
  pred <- bnlearn:::predict.bn.fit(
    fit,
    node = "Y",
    data = newdata,
    prob = TRUE,
    method = "exact"
  )
  
  list(
    class = pred,
    prob = t(
      attr(
        pred,
        "prob"
      )
    )
  )
}


res_M1 <- predict_exact(
  fit_M1,
  profiles
)

res_M2 <- predict_exact(
  fit_M2,
  profiles
)

res_M3 <- predict_exact(
  fit_M3,
  profiles
)

res_M4 <- predict_exact(
  fit_M4,
  profiles
)

res_M5 <- predict_exact(
  fit_M5,
  profiles
)



# ============================================================
# Predicted class comparison
# ============================================================

class_compare <- data.frame(
  profiles,
  M1 = as.character(
    res_M1$class
  ),
  M2 = as.character(
    res_M2$class
  ),
  M3 = as.character(
    res_M3$class
  ),
  M4 = as.character(
    res_M4$class
  ),
  M5 = as.character(
    res_M5$class
  )
)

class_compare



# ============================================================
# Posterior probability comparison
# ============================================================

colnames(
  res_M1$prob
) <- c(
  "M1_A",
  "M1_B",
  "M1_C"
)

colnames(
  res_M2$prob
) <- c(
  "M2_A",
  "M2_B",
  "M2_C"
)

colnames(
  res_M3$prob
) <- c(
  "M3_A",
  "M3_B",
  "M3_C"
)

colnames(
  res_M4$prob
) <- c(
  "M4_A",
  "M4_B",
  "M4_C"
)

colnames(
  res_M5$prob
) <- c(
  "M5_A",
  "M5_B",
  "M5_C"
)


prob_compare <- data.frame(
  profiles,
  round(
    res_M1$prob,
    4
  ),
  round(
    res_M2$prob,
    4
  ),
  round(
    res_M3$prob,
    4
  ),
  round(
    res_M4$prob,
    4
  ),
  round(
    res_M5$prob,
    4
  )
)

prob_compare



# ============================================================
# Confusion matrices and classification accuracy
# ============================================================

# Use only X1, X2, X3 as predictors.
# Do not include Y in the new-data input.

X_data <- dat[
  ,
  c(
    "X1",
    "X2",
    "X3"
  )
]


# ------------------------------------------------------------
# M1
# ------------------------------------------------------------

pred_data_M1 <- bnlearn:::predict.bn.fit(
  fit_M1,
  node = "Y",
  data = X_data,
  prob = TRUE,
  method = "exact"
)


# ------------------------------------------------------------
# M2
# ------------------------------------------------------------

pred_data_M2 <- bnlearn:::predict.bn.fit(
  fit_M2,
  node = "Y",
  data = X_data,
  prob = TRUE,
  method = "exact"
)


# ------------------------------------------------------------
# M3
# ------------------------------------------------------------

pred_data_M3 <- bnlearn:::predict.bn.fit(
  fit_M3,
  node = "Y",
  data = X_data,
  prob = TRUE,
  method = "exact"
)


# ------------------------------------------------------------
# M4
# ------------------------------------------------------------

pred_data_M4 <- bnlearn:::predict.bn.fit(
  fit_M4,
  node = "Y",
  data = X_data,
  prob = TRUE,
  method = "exact"
)


# ------------------------------------------------------------
# M5
# ------------------------------------------------------------

pred_data_M5 <- bnlearn:::predict.bn.fit(
  fit_M5,
  node = "Y",
  data = X_data,
  prob = TRUE,
  method = "exact"
)



# ============================================================
# Confusion matrices
# ============================================================

cm_M1 <- table(
  Actual = dat$Y,
  Predicted = pred_data_M1
)

cm_M2 <- table(
  Actual = dat$Y,
  Predicted = pred_data_M2
)

cm_M3 <- table(
  Actual = dat$Y,
  Predicted = pred_data_M3
)

cm_M4 <- table(
  Actual = dat$Y,
  Predicted = pred_data_M4
)

cm_M5 <- table(
  Actual = dat$Y,
  Predicted = pred_data_M5
)


cm_M1
cm_M2
cm_M3
cm_M4
cm_M5



# ============================================================
# Accuracy
# ============================================================

accuracy_M1 <- mean(
  pred_data_M1 == dat$Y
)

accuracy_M2 <- mean(
  pred_data_M2 == dat$Y
)

accuracy_M3 <- mean(
  pred_data_M3 == dat$Y
)

accuracy_M4 <- mean(
  pred_data_M4 == dat$Y
)

accuracy_M5 <- mean(
  pred_data_M5 == dat$Y
)


accuracy_table <- data.frame(
  Model = c(
    "M1",
    "M2",
    "M3",
    "M4",
    "M5"
  ),
  
  Accuracy = c(
    accuracy_M1,
    accuracy_M2,
    accuracy_M3,
    accuracy_M4,
    accuracy_M5
  )
)

accuracy_table



# ============================================================
# Compare where models disagree
# ============================================================

class_compare


# Number of different predictions among models for each profile

class_compare$Number_of_classes <-
  apply(
    class_compare[
      ,
      c(
        "M1",
        "M2",
        "M3",
        "M4",
        "M5"
      )
    ],
    1,
    function(x) {
      length(
        unique(x)
      )
    }
  )


# Profiles where models disagree

class_compare[
  class_compare$Number_of_classes > 1,
]



# ============================================================
# Find ties / near ties
# ============================================================

top2_gap <- function(prob_matrix) {
  
  apply(
    prob_matrix,
    1,
    function(x) {
      
      s <- sort(
        x,
        decreasing = TRUE
      )
      
      s[1] - s[2]
    }
  )
}


gap_M1 <- top2_gap(
  res_M1$prob
)

gap_M2 <- top2_gap(
  res_M2$prob
)

gap_M3 <- top2_gap(
  res_M3$prob
)

gap_M4 <- top2_gap(
  res_M4$prob
)

gap_M5 <- top2_gap(
  res_M5$prob
)


gap_table <- data.frame(
  profiles,
  M1 = gap_M1,
  M2 = gap_M2,
  M3 = gap_M3,
  M4 = gap_M4,
  M5 = gap_M5
)


# Round only numeric columns

gap_table_rounded <- gap_table

gap_table_rounded[
  c(
    "M1",
    "M2",
    "M3",
    "M4",
    "M5"
  )
] <-
  round(
    gap_table_rounded[
      c(
        "M1",
        "M2",
        "M3",
        "M4",
        "M5"
      )
    ],
    4
  )

gap_table_rounded


# Most ambiguous profile under each model

which.min(
  gap_M1
)

which.min(
  gap_M2
)

which.min(
  gap_M3
)

which.min(
  gap_M4
)

which.min(
  gap_M5
)



# ============================================================
# Optional: inspect the M1 tie at profile 4
# ============================================================

options(
  digits = 15
)

res_M1$prob[
  4,
]


# Return printing to a more normal number of digits if desired
options(
  digits = 7
)


# Profile 4:
# X1 = high
# X2 = high
# X3 = low
#
# Examine real observations having this profile

idx4 <-
  dat$X1 == "high" &
  dat$X2 == "high" &
  dat$X3 == "low"


table(
  dat$Y[idx4]
)


table(
  Actual = dat$Y[idx4],
  M1_Prediction = pred_data_M1[idx4]
)


# Compare M1 and M2 predictions for the original sample

which(
  pred_data_M1 != pred_data_M2
)

data.frame(
  Y = dat$Y,
  X1 = dat$X1,
  X2 = dat$X2,
  X3 = dat$X3,
  M1 = pred_data_M1,
  M2 = pred_data_M2
)[
  pred_data_M1 != pred_data_M2,
]

# ============================================================
# B4.3 Comparison by log marginal likelihood
# ============================================================

# ------------------------------------------------------------
# 1. Compute log marginal likelihood for each model
# ------------------------------------------------------------

logML_M1 <- score(
  M1,
  data = dat,
  type = "bde",
  iss = 12
)

logML_M2 <- score(
  M2,
  data = dat,
  type = "bde",
  iss = 12
)

logML_M3 <- score(
  M3,
  data = dat,
  type = "bde",
  iss = 12
)

logML_M4 <- score(
  M4,
  data = dat,
  type = "bde",
  iss = 12
)

logML_M5 <- score(
  M5,
  data = dat,
  type = "bde",
  iss = 12
)


# Put the five log marginal likelihoods together
logML <- c(
  M1 = logML_M1,
  M2 = logML_M2,
  M3 = logML_M3,
  M4 = logML_M4,
  M5 = logML_M5
)

logML


# ------------------------------------------------------------
# 2. Node-wise contributions
#    Example: M3
# ------------------------------------------------------------

node_score_M3 <- score(
  M3,
  data = dat,
  type = "bde",
  iss = 12,
  by.node = TRUE
)

node_score_M3


# Check that node contributions sum to the total score
sum(node_score_M3)

logML_M3


# ------------------------------------------------------------
# 3. Verify one node contribution by hand
#    Choose Y because Y has no parents
# ------------------------------------------------------------

# For Y:
# prior = Dirichlet(4, 4, 4)
# observed counts = (333, 380, 164)
#
# log p(D_Y | M)
# = log Gamma(12)
#   - log Gamma(12 + 877)
#   + log Gamma(337) - log Gamma(4)
#   + log Gamma(384) - log Gamma(4)
#   + log Gamma(168) - log Gamma(4)

log_Y_hand <-
  lgamma(12) -
  lgamma(12 + 877) +
  lgamma(337) - lgamma(4) +
  lgamma(384) - lgamma(4) +
  lgamma(168) - lgamma(4)

log_Y_hand


# Compare with bnlearn's node contribution
node_score_M3["Y"]


# Difference should be approximately zero
log_Y_hand - node_score_M3["Y"]


# ------------------------------------------------------------
# 4. Convert log marginal likelihoods to posterior
#    model probabilities
# ------------------------------------------------------------

# Equal prior probabilities:
# P(M1) = ... = P(M5) = 1/5
#
# Since the priors are equal,
# P(Mg | D) is proportional to p(D | Mg).

# Numerical stabilization:
logML_shifted <- logML - max(logML)

model_prob <- exp(logML_shifted) /
  sum(exp(logML_shifted))

model_prob


# ------------------------------------------------------------
# 5. Create model comparison table
# ------------------------------------------------------------

model_comparison <- data.frame(
  Model = names(logML),
  Log_Marginal_Likelihood = as.numeric(logML),
  Posterior_Model_Probability = as.numeric(model_prob)
)

model_comparison


# Optional: sort from most supported to least supported
model_comparison_sorted <- model_comparison[
  order(
    model_comparison$Posterior_Model_Probability,
    decreasing = TRUE
  ),
]

model_comparison_sorted


# ------------------------------------------------------------
# 6. Compare with classification accuracy from B4.2(c)
# ------------------------------------------------------------

model_final_comparison <- merge(
  model_comparison,
  accuracy_table,
  by = "Model"
)

model_final_comparison


# Optional: sort by posterior model probability
model_final_comparison <- model_final_comparison[
  order(
    model_final_comparison$Posterior_Model_Probability,
    decreasing = TRUE
  ),
]

model_final_comparison

