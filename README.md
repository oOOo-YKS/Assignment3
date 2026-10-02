Assignment 3 - Part B workspace

START
1. Open Assignment3.Rproj in RStudio.
2. Open 00_setup.R and run it. Required packages are prepared in .Rlibrary.
   On another computer, this script installs any missing packages from CRAN.
3. Open 01_part_b.R. The import and input checks are ready.
4. Complete its TODOs in order: B1, B2, B3, B4, Interpretation.
5. Write reasoning and hand calculations in notes/Part_B_working_notes.txt.

FOLDERS
data/         Original Excel and a direct CSV export of its Data sheet.
references/   Assignment PDF and Lecture Set 4. The other lectures remain
              in the parent course folder.
notes/        Your working derivations and interpretations.
outputs/      Tables, figures, and reproducibility logs you generate.
.Rlibrary/    Local R dependencies. Not part of your submitted assignment.

WORKING RULES
- Use relative paths from the project root. Do not edit the raw data.
- Use alpha=12 except for the specified sensitivity analysis.
- Set seeds for simulation and document how you resolve prediction ties.
- Keep B2 hand calculations alongside the corresponding software checks.
- Label sample-internal accuracy correctly if no held-out data is used.
- Save package versions and keep one final script that reproduces the report.

The earlier completed learning draft and solution are not copied here, so
this folder starts with exercises for your own analysis.

READING
Set 4 pp. 15-24: DAGs and classification.
Set 4 pp. 26-33: counts, parameters, Dirichlet priors and updating.
Set 4 pp. 34-38: a worked classification example.
Set 4 pp. 39-44: model evidence and comparison.

DELIVERABLES
The assignment ultimately requires a PDF write-up and one commented R script.
This starter is for Part B; it is not yet a completed submission.
