# Assignment 3: Bayesian Networks with bnlearn
# Kai - Part B analysis and supporting Part A demonstrations
# Input: data/carrier_data.csv (877 original observations).
# Run: open Assignment3.Rproj, then source("Kai_Assignment3.R").
# Alternatively: Rscript Kai_Assignment3.R
# Required packages: bnlearn and gRain.
# Install once if needed: install.packages(c("bnlearn", "gRain"))
# No dependency on 00_setup.R, an Rmd file, or saved workspace objects.
# Output: submission_results/ with tables, figures, and a complete run log.
# Accuracy is in-sample. Ties within 1e-12 choose A, then B, then C.

run_assignment <- function() {
  # Find data from the project directory or beside the script.
  project_dir <- getwd()
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    project_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1])))
  } else {
    for (frame in sys.frames()) {
      if (!is.null(frame$ofile)) {
        project_dir <- dirname(normalizePath(frame$ofile))
      }
    }
  }
  data_file <- file.path(project_dir, "data", "carrier_data.csv")
  if (!file.exists(data_file)) stop("Missing input: ", data_file)

  # Reuse the local project library if present; otherwise use normal libraries.
  old_libs <- .libPaths()
  on.exit(.libPaths(old_libs), add = TRUE)
  local_lib <- file.path(project_dir, ".Rlibrary")
  if (dir.exists(local_lib)) .libPaths(c(local_lib, .libPaths()))
  for (pkg in c("bnlearn", "gRain")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Install the required package first: install.packages('", pkg, "')")
    }
  }
  library(bnlearn)
  set.seed(20261002)

  output_dir <- file.path(project_dir, "submission_results")
  dir.create(output_dir, showWarnings = FALSE)
  table_dir <- file.path(output_dir, "tables")
  figure_dir <- file.path(output_dir, "figures")
  dir.create(table_dir, showWarnings = FALSE)
  dir.create(figure_dir, showWarnings = FALSE)

  original_sinks <- sink.number()
  sink(file.path(output_dir, "analysis_log.txt"), split = TRUE)
  on.exit({while (sink.number() > original_sinks) sink()}, add = TRUE)
  cat("R:", as.character(getRversion()), "\n")
  cat("bnlearn:", as.character(packageVersion("bnlearn")), "\n")
  cat("gRain:", as.character(packageVersion("gRain")), "\n")
  cat("Seed: 20261002; global precision: 12 except B3.\n")

  # Display rounded tables, but export unrounded numeric values.
  table_number <- 0L
  current_section <- ""
  show_table <- function(x, digits = 6, caption = NULL, ...) {
    table_number <<- table_number + 1L
    if (!is.null(caption)) cat("\n", caption, "\n", sep = "")
    view <- x
    if (is.data.frame(view)) {
      for (j in seq_along(view)) {
        if (is.numeric(view[[j]])) view[[j]] <- round(view[[j]], digits)
      }
    } else if (is.numeric(view)) view <- round(view, digits)
    print(view)
    if (inherits(x, "table") && length(dim(x)) == 2L) {
      saved <- as.data.frame.matrix(x)
      saved <- data.frame(Row = rownames(saved), saved, row.names = NULL)
    } else saved <- as.data.frame(x)
    write.csv(saved, file.path(table_dir,
      sprintf("%02d_%s.csv", table_number, current_section)), row.names = FALSE)
    invisible(x)
  }

  # READ-AND-RECODE -----------------------------------
  current_section <- "read-and-recode"
  cat("\n===== read-and-recode =====\n")
  raw <- read.csv(data_file)
  stopifnot(setequal(names(raw), c("Y", "X1", "X2", "X3")),
            nrow(raw) == 877L, !anyNA(raw), all(raw$Y %in% 1:3))
  for (v in c("X1", "X2", "X3")) stopifnot(all(raw[[v]] %in% 0:1))
  
  d <- raw
  
  d$Y <- factor(
    raw$Y,
    levels = c(1, 2, 3),
    labels = c("A", "B", "C")
  )
  
  d$X1 <- factor(
    raw$X1, 
    levels = c(0, 1), 
    labels = c("low", "high")
  )
  
  d$X2 <- factor(
    raw$X2, 
    levels = c(0, 1), 
    labels = c("low", "high")
  )
  d$X3 <- factor(
    raw$X3, 
    levels = c(0, 1), 
    labels = c("low", "high")
  )
  
  stopifnot(nrow(d) == 877, !anyNA(d))
  show_table(head(d), caption = "First six observations after recoding")

  # FOUR-WAY-DISPLAY -----------------------------------
  current_section <- "four-way-display"
  cat("\n===== four-way-display =====\n")
  four_way <- xtabs(~ Y + X1 + X2 + X3, data = d)
  compact_counts <- ftable(four_way, row.vars = c("X1", "X2", "X3"), col.vars = "Y")
  print(compact_counts)
  stopifnot(sum(four_way) == nrow(d))

  # CROSS-TABULATIONS -----------------------------------
  current_section <- "cross-tabulations"
  cat("\n===== cross-tabulations =====\n")
  y_x1 <- table(Y = d$Y, X1 = d$X1)
  y_x2 <- table(Y = d$Y, X2 = d$X2)
  y_x3 <- table(Y = d$Y, X3 = d$X3)
  
  show_table(y_x1, caption = "Y and reputation satisfaction (X1)")
  show_table(y_x2, caption = "Y and price satisfaction (X2)")
  show_table(y_x3, caption = "Y and service variety satisfaction (X3)")

  # WITHIN-PROVIDER-PROPORTIONS -----------------------------------
  current_section <- "within-provider-proportions"
  cat("\n===== within-provider-proportions =====\n")
  high_proportions <- data.frame(
    Attribute = c("Reputation (X1)", "Price (X2)", "Service variety (X3)"),
    A = c(prop.table(y_x1, 1)["A", "high"], prop.table(y_x2, 1)["A", "high"], prop.table(y_x3, 1)["A", "high"]),
    B = c(prop.table(y_x1, 1)["B", "high"], prop.table(y_x2, 1)["B", "high"], prop.table(y_x3, 1)["B", "high"]),
    C = c(prop.table(y_x1, 1)["C", "high"], prop.table(y_x2, 1)["C", "high"], prop.table(y_x3, 1)["C", "high"])
  )
  show_table(high_proportions, digits = 4,
               caption = "Proportion reporting high satisfaction within each provider group")

  # SUPPLEMENTARY-LOGISTIC-MODELS -----------------------------------
  current_section <- "supplementary-logistic-models"
  cat("\n===== supplementary-logistic-models =====\n")
  model_1 <- glm(I(Y == "A") ~ X1 + X2 + X3,
                 data = d, family = binomial())
  
  model_2 <- glm(I(Y == "B") ~ X1 + X2 + X3,
                 data = d, family = binomial())
  
  model_3 <- glm(I(Y == "C") ~ X1 + X2 + X3,
                 data = d, family = binomial())
  
  # Compare coefficients across the three models
  print(round(cbind(
    A = coef(model_1),
    B = coef(model_2),
    C = coef(model_3)
  ), 3))

  # NAIVE-BAYES-DAG -----------------------------------
  current_section <- "naive-bayes-dag"
  cat("\n===== naive-bayes-dag =====\n")
  png(file.path(figure_dir, "naive-bayes-dag.png"), width = 1800, height = 1200, res = 180)
  nb <- naive.bayes(d, training = "Y")
  plot(nb)
  dev.off()

  # HAND-ARITHMETIC -----------------------------------
  current_section <- "hand-arithmetic"
  cat("\n===== hand-arithmetic =====\n")
  hand_weights <- c(
    A = (337/889) * (132/337) * (198/337) * (174/337),
    B = (384/889) * (249/384) * (244/384) * (208/384),
    C = (168/889) * (54/168) * (106/168) * (75/168)
  )
  hand_probs <- hand_weights / sum(hand_weights)
  show_table(data.frame(Provider = names(hand_probs),
                           Weight = as.numeric(hand_weights),
                           Probability = as.numeric(hand_probs)), digits = 6)

  # B2-4-FIT -----------------------------------
  current_section <- "b2-4-fit"
  cat("\n===== b2-4-fit =====\n")
  nb <- naive.bayes(d, training = "Y")
  
  nb_fit <- bn.fit(
    nb,
    data = d,
    method = "bayes",
    iss = 12
  )
  
  # Conditional probability tables: posterior means
  print(coef(nb_fit))

  # B2-4-POSTERIOR-PARAMETERS -----------------------------------
  current_section <- "b2-4-posterior-parameters"
  cat("\n===== b2-4-posterior-parameters =====\n")
  post_Y  <- table(d$Y) + 4
  post_X1 <- table(Y = d$Y, X1 = d$X1) + 2
  post_X2 <- table(Y = d$Y, X2 = d$X2) + 2
  post_X3 <- table(Y = d$Y, X3 = d$X3) + 2
  
  print(post_Y)
  print(post_X1)
  print(post_X2)
  print(post_X3)

  # B2-4-PREDICT -----------------------------------
  current_section <- "b2-4-predict"
  cat("\n===== b2-4-predict =====\n")
  customer <- data.frame(
    X1 = factor("high", levels = levels(d$X1)),
    X2 = factor("low",  levels = levels(d$X2)),
    X3 = factor("high", levels = levels(d$X3))
  )
  
  prediction <- predict(
    nb_fit,
    data = customer,
    prob = TRUE
  )
  
  print(prediction)
  software_probs <- attr(prediction, "prob")
  print(round(software_probs, 6))
  stopifnot(max(abs(software_probs[c("A", "B", "C"), 1] - hand_probs)) < 1e-10)

  # B2-4-CPQUERY -----------------------------------
  current_section <- "b2-4-cpquery"
  cat("\n===== b2-4-cpquery =====\n")
  set.seed(20261002)
  
  query_probs <- c(
    A = cpquery(
      nb_fit,
      event = (Y == "A"),
      evidence = (X1 == "high" & X2 == "low" & X3 == "high"),
      method = "ls",
      n = 1000000
    ),
    B = cpquery(
      nb_fit,
      event = (Y == "B"),
      evidence = (X1 == "high" & X2 == "low" & X3 == "high"),
      method = "ls",
      n = 1000000
    ),
    C = cpquery(
      nb_fit,
      event = (Y == "C"),
      evidence = (X1 == "high" & X2 == "low" & X3 == "high"),
      method = "ls",
      n = 1000000
    )
  )
  
  comparison <- data.frame(
    Provider = c("A", "B", "C"),
    Predict = as.numeric(software_probs[c("A", "B", "C"), 1]),
    Cpquery = as.numeric(query_probs)
  )
  
  show_table(comparison, digits = 6)

  # B3-PRIOR-SENSITIVITY -----------------------------------
  current_section <- "b3-prior-sensitivity"
  cat("\n===== b3-prior-sensitivity =====\n")
  alpha_values <- c(1, 12, 100, 1000)
  
  sensitivity <- data.frame(
    alpha = alpha_values,
    A = NA_real_,
    B = NA_real_,
    C = NA_real_
  )
  
  for (i in seq_along(alpha_values)) {
    fit <- bn.fit(
      nb,
      data = d,
      method = "bayes",
      iss = alpha_values[i]
    )
  
    pred <- predict(fit, data = customer, prob = TRUE)
  
    sensitivity[i, c("A", "B", "C")] <-
      as.numeric(attr(pred, "prob")[c("A", "B", "C"), 1])
  }
  
  show_table(sensitivity, digits = 6)

  # B4-MODELS -----------------------------------
  current_section <- "b4-models"
  cat("\n===== b4-models =====\n")
  M1 <- model2network("[Y][X1|Y][X2|Y:X1][X3|Y:X1:X2]")
  M2 <- model2network("[Y][X1|Y][X2|Y][X3|Y]")
  M3 <- model2network("[Y][X1|Y][X2|Y:X1][X3|Y]")
  M4 <- model2network("[Y][X1|Y][X2|Y][X3|Y:X1]")
  M5 <- model2network("[Y][X1|Y][X2|Y][X3|Y:X2]")
  
  fit1 <- bn.fit(M1, data = d, method = "bayes", iss = 12)
  fit2 <- bn.fit(M2, data = d, method = "bayes", iss = 12)
  fit3 <- bn.fit(M3, data = d, method = "bayes", iss = 12)
  fit4 <- bn.fit(M4, data = d, method = "bayes", iss = 12)
  fit5 <- bn.fit(M5, data = d, method = "bayes", iss = 12)

  # B4-PLOTS -----------------------------------
  current_section <- "b4-plots"
  cat("\n===== b4-plots =====\n")
  png(file.path(figure_dir, "b4-plots.png"), width = 1800, height = 1200, res = 180)
  par(mfrow = c(2, 3))
  
  plot(M1, main = "M1")
  plot(M2, main = "M2")
  plot(M3, main = "M3")
  plot(M4, main = "M4")
  plot(M5, main = "M5")
  
  par(mfrow = c(1, 1))
  dev.off()

  # B4-PROFILES -----------------------------------
  current_section <- "b4-profiles"
  cat("\n===== b4-profiles =====\n")
  profiles <- data.frame(
    X1 = c("low", "low", "low", "low",
           "high", "high", "high", "high"),
  
    X2 = c("low", "low", "high", "high",
           "low", "low", "high", "high"),
  
    X3 = c("low", "high", "low", "high",
           "low", "high", "low", "high")
  )
  
  profiles$X1 <- factor(profiles$X1, levels = levels(d$X1))
  profiles$X2 <- factor(profiles$X2, levels = levels(d$X2))
  profiles$X3 <- factor(profiles$X3, levels = levels(d$X3))
  
  show_table(profiles)

  # B4-METHODS -----------------------------------
  current_section <- "b4-methods"
  cat("\n===== b4-methods =====\n")
  compare_methods <- function(fitted_model) {
  
    pred_parents <- predict(
      fitted_model, node = "Y", data = profiles,
      method = "parents", prob = TRUE
    )
  
    set.seed(20261002)
    pred_lw <- predict(
      fitted_model, node = "Y", data = profiles,
      method = "bayes-lw", prob = TRUE, n = 100000
    )
  
    pred_exact <- predict(
      fitted_model, node = "Y", data = profiles,
      method = "exact", prob = TRUE
    )
  
    # Probability matrices have classes in rows.
    # t() transposes them so each customer profile becomes a row.
    p_parents <- t(attr(pred_parents, "prob")[c("A", "B", "C"), ])
    p_lw <- t(attr(pred_lw, "prob")[c("A", "B", "C"), ])
    p_exact <- t(attr(pred_exact, "prob")[c("A", "B", "C"), ])
  
    result <- rbind(
      data.frame(Method = "parents", profiles, p_parents),
      data.frame(Method = "bayes-lw", profiles, p_lw),
      data.frame(Method = "exact", profiles, p_exact)
    )
  
    return(result)
  }
  
  methods1 <- compare_methods(fit1)
  methods2 <- compare_methods(fit2)
  methods3 <- compare_methods(fit3)
  methods4 <- compare_methods(fit4)
  methods5 <- compare_methods(fit5)
  
  show_table(methods1, digits = 4, caption = "M1")
  show_table(methods2, digits = 4, caption = "M2")
  show_table(methods3, digits = 4, caption = "M3")
  show_table(methods4, digits = 4, caption = "M4")
  show_table(methods5, digits = 4, caption = "M5")

  # B4-EXACT -----------------------------------
  current_section <- "b4-exact"
  cat("\n===== b4-exact =====\n")
  get_results <- function(fitted_model, new_data) {
  
    prediction <- predict(
      fitted_model, node = "Y", data = new_data,
      method = "exact", prob = TRUE
    )
  
    probabilities <- t(
      attr(prediction, "prob")[c("A", "B", "C"), , drop = FALSE]
    )
  
    predicted_class <- character(nrow(new_data))
    classes <- c("A", "B", "C")
  
    for (i in seq_len(nrow(new_data))) {
      highest <- max(probabilities[i, ])
  
      # Find classes tied for the highest probability.
      candidates <- which(probabilities[i, ] >= highest - 1e-12)
  
      # Choose the first: A, then B, then C.
      predicted_class[i] <- classes[candidates[1]]
    }
  
    result <- data.frame(
      new_data,
      probabilities,
      Predicted = predicted_class
    )
  
    return(result)
  }
  
  result1 <- get_results(fit1, profiles)
  result2 <- get_results(fit2, profiles)
  result3 <- get_results(fit3, profiles)
  result4 <- get_results(fit4, profiles)
  result5 <- get_results(fit5, profiles)
  
  # Each cell shows (A, B, C) probabilities and the prediction.
  format_result <- function(result) {
    sprintf(
      "(%.4f, %.4f, %.4f) -> %s",
      result$A, result$B, result$C, result$Predicted
    )
  }
  
  comparison <- data.frame(
    profiles,
    M1 = format_result(result1),
    M2 = format_result(result2),
    M3 = format_result(result3),
    M4 = format_result(result4),
    M5 = format_result(result5)
  )
  
  show_table(comparison)

  # B4-ACCURACY -----------------------------------
  current_section <- "b4-accuracy"
  cat("\n===== b4-accuracy =====\n")
  # Only attributes are supplied as prediction inputs.
  attributes <- d[, c("X1", "X2", "X3")]
  
  training1 <- get_results(fit1, attributes)
  training2 <- get_results(fit2, attributes)
  training3 <- get_results(fit3, attributes)
  training4 <- get_results(fit4, attributes)
  training5 <- get_results(fit5, attributes)
  
  # Keep all three prediction categories, even if one is never selected.
  pred1 <- factor(training1$Predicted, levels = c("A", "B", "C"))
  pred2 <- factor(training2$Predicted, levels = c("A", "B", "C"))
  pred3 <- factor(training3$Predicted, levels = c("A", "B", "C"))
  pred4 <- factor(training4$Predicted, levels = c("A", "B", "C"))
  pred5 <- factor(training5$Predicted, levels = c("A", "B", "C"))
  
  confusion1 <- table(Actual = d$Y, Predicted = pred1)
  show_table(confusion1, caption = "M1: rows are actual classes; columns are predicted classes")
  confusion2 <- table(Actual = d$Y, Predicted = pred2)
  show_table(confusion2, caption = "M2: rows are actual classes; columns are predicted classes")
  confusion3 <- table(Actual = d$Y, Predicted = pred3)
  show_table(confusion3, caption = "M3: rows are actual classes; columns are predicted classes")
  confusion4 <- table(Actual = d$Y, Predicted = pred4)
  show_table(confusion4, caption = "M4: rows are actual classes; columns are predicted classes")
  confusion5 <- table(Actual = d$Y, Predicted = pred5)
  show_table(confusion5, caption = "M5: rows are actual classes; columns are predicted classes")
  
  accuracy1 <- mean(pred1 == d$Y)
  accuracy2 <- mean(pred2 == d$Y)
  accuracy3 <- mean(pred3 == d$Y)
  accuracy4 <- mean(pred4 == d$Y)
  accuracy5 <- mean(pred5 == d$Y)
  
  accuracy_table <- data.frame(
    Model = c("M1", "M2", "M3", "M4", "M5"),
    Accuracy = c(accuracy1, accuracy2, accuracy3, accuracy4, accuracy5)
  )
  
  show_table(accuracy_table, digits = 6)

  # B4-MODEL-COMPARISON -----------------------------------
  current_section <- "b4-model-comparison"
  cat("\n===== b4-model-comparison =====\n")
  score1 <- score(M1, data = d, type = "bde", iss = 12)
  score2 <- score(M2, data = d, type = "bde", iss = 12)
  score3 <- score(M3, data = d, type = "bde", iss = 12)
  score4 <- score(M4, data = d, type = "bde", iss = 12)
  score5 <- score(M5, data = d, type = "bde", iss = 12)
  
  log_scores <- c(score1, score2, score3, score4, score5)
  
  # Equal model priors cancel when we normalize.
  weights <- exp(log_scores - max(log_scores))
  model_probabilities <- weights / sum(weights)
  
  model_comparison <- data.frame(
    Model = c("M1", "M2", "M3", "M4", "M5"),
    Parameters = c(
      nparams(fit1), nparams(fit2), nparams(fit3),
      nparams(fit4), nparams(fit5)
    ),
    Log_marginal_likelihood = log_scores,
    Posterior_probability = formatC(
      model_probabilities, format = "e", digits = 3
    ),
    Accuracy = c(accuracy1, accuracy2, accuracy3, accuracy4, accuracy5)
  )
  
  show_table(model_comparison, digits = 6)
  
  # Strength of evidence for M1 over M5, the second-best model
  print(score1 - score5)
  print(exp(score1 - score5))

  # B4-HAND-CHECK -----------------------------------
  current_section <- "b4-hand-check"
  cat("\n===== b4-hand-check =====\n")
  # Prior: Dir(4, 4, 4)
  # Counts: (333, 380, 164)
  # Posterior: Dir(337, 384, 168)
  
  manual_Y <- lgamma(12) - lgamma(889) +
    lgamma(337) + lgamma(384) + lgamma(168) -
    3 * lgamma(4)
  
  node_scores <- score(
    M2, data = d, type = "bde", iss = 12, by.node = TRUE
  )
  
  print(manual_Y)
  print(node_scores["Y"])
  
  # The sum of node contributions equals the whole-network score.
  print(sum(node_scores))
  print(score2)

  # FINAL-CHECKS -----------------------------------
  current_section <- "final-checks"
  cat("\n===== final-checks =====\n")
  stopifnot(abs(manual_Y - unname(node_scores["Y"])) < 1e-8,
            abs(sum(node_scores) - score2) < 1e-8,
            max(abs(c(accuracy1, accuracy2, accuracy3, accuracy4, accuracy5) - 450/877)) < 1e-12,
            abs(sum(model_probabilities) - 1) < 1e-12)

  # Supplementary exports and checks -------------------------------------
  # Export all 24 counts and exact probability vectors as numeric tables.
  write.csv(as.data.frame(four_way), file.path(table_dir, "four_way_counts.csv"), row.names = FALSE)
  write.csv(result1, file.path(table_dir, "M1_exact.csv"), row.names = FALSE)
  write.csv(result2, file.path(table_dir, "M2_exact.csv"), row.names = FALSE)
  write.csv(result3, file.path(table_dir, "M3_exact.csv"), row.names = FALSE)
  write.csv(result4, file.path(table_dir, "M4_exact.csv"), row.names = FALSE)
  write.csv(result5, file.path(table_dir, "M5_exact.csv"), row.names = FALSE)

  # Verify all local BDe contributions by hand from the observed counts.
  manual_node_score <- function(node, parent_names, alpha = 12) {
    counts <- table(d[, c(node, parent_names), drop = FALSE])
    C <- nlevels(d[[node]])
    Q <- length(counts) / C
    count_matrix <- matrix(as.numeric(counts), nrow = C)
    a <- alpha / (C * Q)
    total <- 0
    for (j in seq_len(Q)) {
      local_counts <- count_matrix[, j]
      total <- total + lgamma(alpha / Q) - lgamma(alpha / Q + sum(local_counts)) +
        sum(lgamma(a + local_counts) - lgamma(a))
    }
    total
  }
  models <- list(M1 = M1, M2 = M2, M3 = M3, M4 = M4, M5 = M5)
  for (m in names(models)) {
    local_scores <- score(models[[m]], data = d, type = "bde", iss = 12, by.node = TRUE)
    for (node in names(d)) {
      manual <- manual_node_score(node, parents(models[[m]], node))
      stopifnot(abs(manual - local_scores[node]) < 1e-8)
    }
  }

  # Reproduce the complete M3 Dirichlet parameters: one Y vector, three X1
  # vectors, six X2 vectors, and three X3 vectors. These are not CPT means.
  cat("\nM3 full Dirichlet posterior parameters\n")
  print(table(d$Y) + 4)
  print(table(Y = d$Y, X1 = d$X1) + 2)
  print(xtabs(~ X2 + Y + X1, data = d) + 1)
  print(table(Y = d$Y, X3 = d$X3) + 2)

  # Part A examples: structure strings, MLE estimates, and BIC scores.
  cat("\nPart A supporting examples\n")
  print(modelstring(M3))
  print(nparams(M2, data = d))
  fit_mle <- bn.fit(M2, data = d, method = "mle")
  print(coef(fit_mle$X1))
  print(coef(fit2$X1))
  bic_scores <- c(
    M1 = score(M1, data = d, type = "bic"),
    M2 = score(M2, data = d, type = "bic"),
    M3 = score(M3, data = d, type = "bic"),
    M4 = score(M4, data = d, type = "bic"),
    M5 = score(M5, data = d, type = "bic")
  )
  print(bic_scores)
  cat("\nAll consistency checks passed.\n")
  print(sessionInfo())
  cat("\nResults saved to:", output_dir, "\n")
  invisible(model_comparison)
}

run_assignment()
