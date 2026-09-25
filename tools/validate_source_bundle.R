# Validate that the generated sourceable distribution matches canonical package R sources.
#
# Run with:
# R.exe --vanilla --slave -f tools/validate_source_bundle.R

package_root <- normalizePath(file.path(getwd()), winslash = "/", mustWork = TRUE)
source_dir <- file.path(package_root, "R")
bundle_root <- file.path(package_root, "deploy", "adamtools.yw")
bundle_r_dir <- file.path(bundle_root, "R")
manifest_path <- file.path(bundle_root, "MANIFEST.csv")

if (!dir.exists(bundle_r_dir) || !file.exists(manifest_path)) {
  stop("Source bundle is missing. Run tools/sync_source_bundle.R first.", call. = FALSE)
}

source_files <- sort(list.files(source_dir, pattern = "\\.R$", full.names = FALSE))
bundle_files <- sort(list.files(bundle_r_dir, pattern = "\\.R$", full.names = FALSE))
if (!identical(source_files, bundle_files)) {
  stop("Source bundle file list differs from canonical package R sources.", call. = FALSE)
}

source_md5 <- unname(tools::md5sum(file.path(source_dir, source_files)))
bundle_md5 <- unname(tools::md5sum(file.path(bundle_r_dir, bundle_files)))
if (!identical(source_md5, bundle_md5)) {
  stop("Source bundle content differs from canonical package R sources. Run tools/sync_source_bundle.R.", call. = FALSE)
}

manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
if (!identical(manifest$file, source_files) || !identical(manifest$md5, source_md5)) {
  stop("Source bundle manifest differs from canonical package R sources. Run tools/sync_source_bundle.R.", call. = FALSE)
}

message("Source bundle matches ", length(source_files), " canonical R source files.")
