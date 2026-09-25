#' Compare two ADaM datasets using diffdf
#'
#' Standardizes the repeated production-vs-QC (or generated-vs-reference)
#' ADaM comparison pattern built on `diffdf::diffdf()`: optional variable
#' attribute stripping before comparison, configurable key variables, and a
#' structured, reusable call so QC programs no longer need to hand-write the
#' same attribute-stripping/comparison boilerplate.
#'
#' @param base A data frame or tibble; the reference/base dataset (e.g. the
#'   production dataset).
#' @param compare A data frame or tibble; the dataset to compare against
#'   `base` (e.g. the independently-programmed QC dataset).
#' @param keys Character vector of key variables used to match records
#'   between `base` and `compare`. If `NULL` (default), `diffdf::diffdf()`
#'   determines its own row matching.
#' @param strip_attrs Logical; strip variable attributes (e.g. `label`,
#'   `format.sas`) from both datasets before comparing, via
#'   [strip_var_attrs()]. Defaults to `FALSE`.
#' @param suppress_warnings Logical; suppress warnings raised during the
#'   comparison (e.g. attribute mismatches unrelated to data content).
#'   Defaults to `TRUE`.
#' @param ... Additional arguments passed through to `diffdf::diffdf()`.
#'
#' @return The `diffdf` comparison object returned by `diffdf::diffdf()`.
#' @export
compare_adam_dataset <- function(base,
                                 compare,
                                 keys = NULL,
                                 strip_attrs = FALSE,
                                 suppress_warnings = TRUE,
                                 ...) {
  if (!inherits(base, "data.frame") || !inherits(compare, "data.frame")) {
    stop("`base` and `compare` must both be data frames or tibbles.", call. = FALSE)
  }

  if (strip_attrs) {
    base <- strip_var_attrs(base)
    compare <- strip_var_attrs(compare)
  }

  diffdf::diffdf(
    base = base,
    compare = compare,
    keys = keys,
    suppress_warnings = suppress_warnings,
    ...
  )
}
