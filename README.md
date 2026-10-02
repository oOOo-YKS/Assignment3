# Assignment 3 — Bayesian Networks in R

A working project for completing **Part B** of the DNSC 8328 Bayesian networks assignment using `bnlearn`. We will use survey data to build and compare five classifiers for preferred long-distance carrier, working through the calculations ourselves before writing up the results.

## Quick start

1. Open [Assignment3.Rproj](Assignment3.Rproj) in RStudio. This sets the working directory to the project folder.
2. Open [01_part_b.R](01_part_b.R).
3. Run the setup and data-import section, then complete the TODOs one section at a time. In RStudio, use **Ctrl+Enter** to run the current line or selection.
4. Record derivations and explanations in [Part B working notes](notes/Part_B_working_notes.txt).

The starter begins with:

```r
source("00_setup.R", encoding = "UTF-8")
```

This loads the project library, installs missing packages (`bnlearn`, `gRain`, and `readxl`), sets the random seed, creates output folders, and records the session information. You can also run [00_setup.R](00_setup.R) separately to check your environment. Installing missing packages requires an internet connection.

> The import and input checks are ready. The analysis sections are exercises to complete; running the starter does not yet produce a finished analysis.

## Project layout

```text
Assignment3/
├── Assignment3.Rproj           # Open this in RStudio
├── README.md                  # Project guide
├── 00_setup.R                 # Packages, seed, folders, and session log
├── 01_part_b.R                 # Main analysis script to complete
├── data/
│   ├── Long_Distance_Carrier Data.xls
│   └── carrier_data.csv
├── references/                # Assignment PDF, Set 4, and supplement
├── notes/                     # Hand calculations and working explanations
├── outputs/
│   ├── tables/
│   ├── figures/
│   └── logs/
└── .Rlibrary/                 # Local packages; excluded from Git
```

Use paths relative to this folder, such as `data/carrier_data.csv`. Keep the source data unchanged and save generated results under `outputs/`.

## Data

The Excel workbook's **Data** sheet contains **877 observations**. The CSV is a direct export of that sheet; either import method in the starter can be used.

| Variable | Meaning | Original coding | Analysis levels |
| --- | --- | --- | --- |
| `Y` | Preferred carrier | `1`, `2`, `3` | `A`, `B`, `C` |
| `X1` | Satisfaction with reputation as an industry leader | `0`, `1` | `low`, `high` |
| `X2` | Satisfaction with price | `0`, `1` | `low`, `high` |
| `X3` | Satisfaction with variety of services | `0`, `1` | `low`, `high` |

Recode all four variables as **factors** before fitting discrete networks. Use the same factor levels and order in training data and prediction profiles.

## Part B workflow

### B1 — Explore the data

- [ ] Import and inspect the data; check coding, row count, and missing values.
- [ ] Recode the four variables as factors.
- [ ] Produce the four-way contingency table and the three `Y`-by-`X` cross-tabulations.
- [ ] Compare appropriate proportions and explain which attribute appears most related to preference.

### B2 — Work through naive Bayes

- [ ] Draw the DAG with `Y` as the class node and write its joint factorization.
- [ ] Define the parameter vectors and identify each node's category count and parent configurations.
- [ ] Write the Dirichlet prior and posterior parameters using **α = 12**.
- [ ] By hand, calculate and normalize the class weights for the **high / low / high** customer.
- [ ] Reproduce the result with `naive.bayes()`, `bn.fit()`, and `predict(..., prob = TRUE)`.
- [ ] Compare with `cpquery()` and explain simulation variability.

Keep the intermediate counts and calculations so that someone else can follow how the final probabilities were obtained.

### B3 — Check prior sensitivity

- [ ] Refit naive Bayes with **α = 1, 12, 100, and 1000**.
- [ ] Tabulate the same customer's class probabilities for each setting.
- [ ] Explain the changes using the posterior mean formula.

### B4 — Compare five classifiers

Every model includes arrows from `Y` to all three attributes. They differ in the dependence allowed among attributes **conditional on `Y`**.

| Model | Attribute structure given `Y` |
| --- | --- |
| M1 | Full dependence among `X1`, `X2`, and `X3` |
| M2 | Mutual independence: naive Bayes |
| M3 | Dependence between `X1` and `X2`; `X3` independent of that pair |
| M4 | Dependence between `X1` and `X3`; `X2` independent of that pair |
| M5 | Dependence between `X2` and `X3`; `X1` independent of that pair |

- [ ] Draw each DAG, write its factorization, and list parent configurations.
- [ ] Build the networks with `model2network()` and fit with `method = "bayes", iss = 12`.
- [ ] Report the complete posterior Dirichlet vectors for at least one non-naive model.
- [ ] Compare `parents`, `bayes-lw`, and `exact`; explain why `parents` is unsuitable here.
- [ ] Use `exact` to obtain class probabilities and predictions for all **eight attribute profiles** under every model.
- [ ] Compare probabilities side by side; identify disagreements, ties, and near ties.
- [ ] Report confusion matrices and accuracy, with the evaluation data and tie rule clearly stated.
- [ ] Calculate log marginal likelihoods with `score(..., type = "bde", iss = 12)` and verify one node's contribution by hand.
- [ ] Convert the scores to posterior model probabilities using equal model priors.
- [ ] Discuss whether the model with the strongest evidence also classifies best.

### Interpretation

- [ ] In at most one page, explain what the results suggest about the attributes and the implications for carriers **A and C**.
- [ ] Distinguish observed associations from causal conclusions and acknowledge limitations in classification performance.

## Analysis conventions

- Use **α = 12** except in B3. In `bnlearn`, the corresponding argument is `iss`.
- Set a seed for stochastic calculations and state how prediction ties are resolved.
- For the three-method comparison, use ordinary networks built with `model2network()`. Naive Bayes classifier objects have their own prediction method.
- Label confusion-matrix axes and report **in-sample accuracy** if fitting and evaluating on the same data.
- Retain full precision for calculations; round only displayed results.
- Keep code in execution order and save package versions in `outputs/logs/session_info.txt`.
- Check functions and arguments against the installed package help, for example `?bn.fit`, `?cpquery`, and `?score`.

## Reading guide

Start with [Lecture Set 4](references/Bayes%20Lecture%20Set%204N.pdf):

| Slides | Topic |
| --- | --- |
| 15–24 | DAGs, conditional independence, factorization, and classification |
| 26–33 | Parameter notation, counts, Dirichlet priors, and posterior updating |
| 34–38 | Worked classification example: a template for B2 |
| 39–44 | Marginal likelihood and model comparison |

Use the [moral graph supplement](references/Supplement%20to%20Set%204%20Moral_graph_criterion.pdf) when checking conditional independence. The [assignment PDF](references/Assignment_bnlearn_R%20version.pdf) is the source of the submission requirements.

## Before submission

The full assignment is due **October 8, 2026, before class**, and requires a **PDF write-up covering Parts A and B** and **one commented R script**. This workspace currently focuses on Part B. The group also presents its work in class; presentation guidelines are to follow.

- [ ] Restart R and run the completed script from top to bottom.
- [ ] Check that every reported number, table, and figure is reproducible.
- [ ] Confirm that the hand calculations agree with the appropriate software results.
- [ ] Add Part A's function guide and check the report against the assignment PDF.
- [ ] Ensure every group member can explain the code and conclusions they submit.

Local packages, RStudio state, and temporary workspace files are not submission materials. The earlier completed learning draft is kept outside this project so this folder can be used for our own analysis.
