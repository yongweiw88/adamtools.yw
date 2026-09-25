# Sourceable deployment loader for restricted R environments.
#
# Usage:
# source("path/to/adamtools.yw/load_adamtools_yw.R")
#
# This bundle is generated from the canonical package source. Do not edit files
# under deploy/ directly; update the package and run tools/sync_source_bundle.R.

.adamtools_bundle_loader <- normalizePath(sys.frame(1L)$ofile, winslash = "/", mustWork = TRUE)
.adamtools_bundle_root <- dirname(.adamtools_bundle_loader)
.adamtools_bundle_r_dir <- file.path(.adamtools_bundle_root, "R")

if (!dir.exists(.adamtools_bundle_r_dir)) {
  stop("The adamtools.yw source bundle R directory is missing.", call. = FALSE)
}

.adamtools_bundle_files <- sort(list.files(
  .adamtools_bundle_r_dir,
  pattern = "\\.R$",
  full.names = TRUE
))
if (length(.adamtools_bundle_files) == 0L) {
  stop("The adamtools.yw source bundle contains no R source files.", call. = FALSE)
}

for (.adamtools_bundle_file in .adamtools_bundle_files) {
  source(.adamtools_bundle_file, local = globalenv())
}

rm(
  .adamtools_bundle_file,
  .adamtools_bundle_files,
  .adamtools_bundle_loader,
  .adamtools_bundle_r_dir,
  .adamtools_bundle_root,
  envir = globalenv()
)
