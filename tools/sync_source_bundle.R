# Generate the sourceable distribution from the canonical package R sources.
#
# Run with:
# R.exe --vanilla --slave -f tools/sync_source_bundle.R

package_root <- normalizePath(file.path(getwd()), winslash = "/", mustWork = TRUE)
source_dir <- file.path(package_root, "R")
bundle_root <- file.path(package_root, "deploy", "adamtools.yw")
bundle_r_dir <- file.path(bundle_root, "R")

if (!dir.exists(source_dir)) {
  stop("Run this script from the adamtools.yw package root.", call. = FALSE)
}

dir.create(bundle_r_dir, recursive = TRUE, showWarnings = FALSE)

source_files <- sort(list.files(source_dir, pattern = "\\.R$", full.names = FALSE))
existing_files <- list.files(bundle_r_dir, pattern = "\\.R$", full.names = FALSE)
stale_files <- setdiff(existing_files, source_files)
if (length(stale_files) > 0L) {
  unlink(file.path(bundle_r_dir, stale_files))
}

copied <- file.copy(
  from = file.path(source_dir, source_files),
  to = file.path(bundle_r_dir, source_files),
  overwrite = TRUE,
  copy.date = TRUE
)
if (!all(copied)) {
  stop("Failed to synchronize one or more R source files into the deployment bundle.", call. = FALSE)
}

manifest <- data.frame(
  file = source_files,
  md5 = unname(tools::md5sum(file.path(source_dir, source_files))),
  stringsAsFactors = FALSE
)
utils::write.csv(
  manifest,
  file = file.path(bundle_root, "MANIFEST.csv"),
  row.names = FALSE,
  quote = TRUE
)

message("Synchronized ", length(source_files), " R source files to ", bundle_root)
