# Open Assignment3.Rproj first, then run this file from the project root.
if (!file.exists("Assignment3.Rproj")) {
  stop("Open Assignment3.Rproj, or set your working directory to Assignment3.")
}
dir.create(".Rlibrary", showWarnings = FALSE)
.libPaths(c(normalizePath(".Rlibrary"), .libPaths()))
required <- c("bnlearn", "gRain", "readxl")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing, lib = ".Rlibrary", repos = "https://cloud.r-project.org")
}
stopifnot(all(vapply(required, requireNamespace, logical(1), quietly = TRUE)))
library(bnlearn)
set.seed(20261002)
for (folder in c("outputs/tables", "outputs/figures", "outputs/logs")) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
}
versions <- vapply(required, function(p) as.character(packageVersion(p)), character(1))
print(versions)
writeLines(capture.output(sessionInfo()), "outputs/logs/session_info.txt")
message("Ready. Open 01_part_b.R and begin with B1.")
