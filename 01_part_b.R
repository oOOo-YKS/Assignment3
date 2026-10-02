# Assignment 3 - Part B working script
# Work through each section yourselves. Keep code in execution order.
# Run from the Assignment3 project root.
source("00_setup.R", encoding = "UTF-8")

# B1 ----------------------------------------------------------------------
# CSV is a direct export of the original workbook's Data sheet.
# Choose ONE import method. Both should give the same observations.
raw <- read.csv("data/carrier_data.csv")
# raw <- as.data.frame(readxl::read_excel(
#   "data/Long_Distance_Carrier Data.xls", sheet = "Data"))

stopifnot(nrow(raw) == 877L,
          setequal(names(raw), c("X1", "X2", "X3", "Y")),
          !anyNA(raw), all(raw$Y %in% 1:3))
for (v in c("X1", "X2", "X3")) stopifnot(all(raw[[v]] %in% 0:1))
str(raw)
head(raw)

# TODO: Copy raw to d; recode Y as factor A/B/C and X1-X3 as low/high.
# TODO: Check str(d), levels, sample size, and missing values.
# TODO: Create the four-way frequency table and Y-by-X cross-tabulations.
# TODO: Compare proportions and explain which attribute appears most related.

# B2: Naive Bayes by hand and in R ------------------------------------------
alpha <- 12
# TODO: Draw Y -> X1, X2, X3; write the joint factorization in your notes.
# TODO: Define the parameter vectors using the lecture's notation.
# TODO: Identify C_i and Q_i; calculate alpha_ijk = alpha/(C_i*Q_i).
# TODO: Tabulate all prior vectors, counts, and posterior Dirichlet vectors.
# TODO: For high/low/high, calculate three weights and normalize by their sum.
# TODO: Build with naive.bayes(), fit with bn.fit(), inspect coef().
# TODO: Obtain predict(..., prob=TRUE); inspect attr(prediction, "prob").
# TODO: Use cpquery() with a seed; compare its estimate with your hand result.
# Hint: naive.bayes objects use a dedicated prediction method. See ?naive.bayes.

# B3: Prior sensitivity ----------------------------------------------------
alpha_values <- c(1, 12, 100, 1000)
# TODO: Refit for each alpha and collect the same customer's A/B/C probabilities.
# TODO: Explain the change using the posterior mean formula, not just the table.

# B4.1: Five structures ----------------------------------------------------
# TODO: Enter the five model strings from your chosen DAGs.
# M1: full attribute dependence given Y; M2: conditional independence.
# M3: X1-X2 linked; M4: X1-X3 linked; M5: X2-X3 linked.
# All five have Y as a parent of X1, X2, X3. Keep each graph acyclic.
# TODO: For each factor, list its parent configurations and free parameters.
# TODO: Build ordinary networks using model2network() for method comparisons.

# B4.2: Estimation and prediction ------------------------------------------
# TODO: Fit all five with method="bayes", iss=12.
# TODO: Report every posterior vector for at least one non-naive model.
# TODO: Build all eight predictor profiles; preserve factor levels.
# TODO: Compare parents, bayes-lw, exact. Set a seed for stochastic results.
# TODO: Explain why parents ignores the attributes when Y has no parents.
# TODO: Use exact thereafter; compare all 40 probability vectors side by side.
# TODO: Define a tie rule; record tied/nearly tied profiles.
# TODO: Produce confusion matrices and accuracy with clearly labelled axes.
# State whether accuracy is in-sample; do not call it test accuracy.
# Do not pass the true Y as a predictor when forming prediction data.

# B4.3: Model comparison ---------------------------------------------------
# TODO: score(..., type="bde", iss=12); inspect by.node=TRUE.
# TODO: Check one node manually using lgamma and the observed counts.
# TODO: Normalize exp(log_score - max(log_score)) under equal model priors.
# TODO: Compare model evidence and classification accuracy; explain differences.

# Interpretation ----------------------------------------------------------
# TODO: At most one page: attributes, implications for A/C, and limitations.
# Distinguish associations from causal claims; discuss per-class performance.

# Final reproducibility ---------------------------------------------------
# TODO: Save named tables and plots under outputs/.
# TODO: Restart R and run this script top-to-bottom after completing all TODOs.
# TODO: Ensure every numerical claim in your write-up is reproduced here.
