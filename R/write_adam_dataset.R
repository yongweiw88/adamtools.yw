#' Write an ADaM dataset to XPT and Parquet formats
#'
#' SKELETON - NOT YET IMPLEMENTED.
#'
#' Writes a single ADaM dataset to both the submission `.xpt` format (via
#' `xportr`) and the internal `.parquet` format (via `arrow`), following the
#' repeated dual-write pattern found across study dataset programs (e.g.
#' `xportr::xportr_write()` immediately followed by `arrow::write_parquet()`).
#' This does not replace `xportr`'s type/length/label/format checks; the
#' dataset passed in should already have those applied. Consider whether
#' `metacore`/`xportr` prep steps belong here or should remain in the caller
#' before wiring this up.
#'
#' @param dataset A data frame or tibble; the ADaM dataset ready for writing.
#' @param domain Dataset domain, e.g. `"ADAE"`. Used for xpt file naming and
#'   passed through to `xportr::xportr_write()`.
#' @param metacore A `metacore` object for the dataset, used for xpt export
#'   metadata. Optional if `xpt` is `FALSE`.
#' @param out_dir Directory to write outputs into.
#' @param xpt Logical; write the `.xpt` file. Defaults to `TRUE`.
#' @param parquet Logical; write the `.parquet` file. Defaults to `TRUE`.
#'
#' @return Invisibly, a list of file paths written.
#' @keywords internal
write_adam_dataset <- function(dataset,
                               domain,
                               metacore = NULL,
                               out_dir,
                               xpt = TRUE,
                               parquet = TRUE) {
  # TODO: validate `dataset`, `domain`, `out_dir` inputs
  # TODO: if xpt, build path <out_dir>/<tolower(domain)>.xpt and call
  #       xportr::xportr_write(dataset, path = ..., metadata = metacore, domain = domain)
  # TODO: if parquet, build path <out_dir>/<tolower(domain)>.parquet and call
  #       arrow::write_parquet(dataset, path)
  # TODO: return list(xpt_path = ..., parquet_path = ...) invisibly
  stop("write_adam_dataset() is a skeleton and not yet implemented.")
}
