#' Derive a Low/High/Normal range indicator from a value and its limits
#'
#' Compares an analysis value against a lower and upper limit and classifies
#' it as below, above, or within range. This formalizes the shape shared by
#' `ANRIND` (source-SDTM-provided reference range, e.g. `LB.LBSTNRLO/HI`) and
#' `A1IND`/`A2IND`/... (study-defined clinical thresholds not present in
#' SDTM source, indexed when a parameter needs more than one threshold set)
#' -- see the ADaM IG conformance note on `ANRLO/ANRHI/ANRIND` vs
#' `A1LO/A1HI/A1IND` for when to use which. Both use the identical
#' value-vs-limits comparison; only the source of the limits, the variable
#' names, and (optionally) the label text differ, all of which are
#' caller-configured here.
#'
#' @param dataset A data frame or tibble.
#' @param aval Name of the analysis value column (unquoted). Defaults to
#'   `AVAL`.
#' @param lo Name of the lower-limit column (unquoted). Defaults to `ANRLO`.
#' @param hi Name of the upper-limit column (unquoted). Defaults to `ANRHI`.
#' @param new_var Name of the new indicator column to create. Defaults to
#'   `"ANRIND"`.
#' @param low_label,high_label,normal_label Character labels assigned when
#'   `aval` is below `lo`, above `hi`, or within `[lo, hi]` respectively.
#'   Default to `"LOW"`/`"HIGH"`/`"NORMAL"` (the `ANRIND` convention); pass
#'   `"Low"`/`"High"`/`"Within Range"` for an `A1IND`-style indicator. A
#'   missing `lo` (or `hi`) only blocks the corresponding Low (or High)
#'   classification -- e.g. a record with only `hi` populated can still be
#'   flagged High but never Low.
#'
#' @return `dataset` with `new_var` added (`NA_character_` when `aval` is
#'   missing, or when neither `lo` nor `hi` is available to compare against).
#' @export
derive_range_indicator <- function(dataset,
                                   aval = AVAL,
                                   lo = ANRLO,
                                   hi = ANRHI,
                                   new_var = "ANRIND",
                                   low_label = "LOW",
                                   high_label = "HIGH",
                                   normal_label = "NORMAL") {
  aval_sym <- rlang::ensym(aval)
  lo_sym <- rlang::ensym(lo)
  hi_sym <- rlang::ensym(hi)

  aval_name <- rlang::as_name(aval_sym)
  lo_name <- rlang::as_name(lo_sym)
  hi_name <- rlang::as_name(hi_sym)

  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }
  missing_cols <- setdiff(c(aval_name, lo_name, hi_name), names(dataset))
  if (length(missing_cols) > 0L) {
    stop("Column(s) not found in `dataset`: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  aval_v <- dataset[[aval_name]]
  lo_v <- dataset[[lo_name]]
  hi_v <- dataset[[hi_name]]

  is_low <- !is.na(aval_v) & !is.na(lo_v) & aval_v < lo_v
  is_high <- !is.na(aval_v) & !is.na(hi_v) & aval_v > hi_v
  has_any_limit <- !is.na(lo_v) | !is.na(hi_v)

  result <- ifelse(
    is.na(aval_v) | !has_any_limit, NA_character_,
    ifelse(is_low, low_label, ifelse(is_high, high_label, normal_label))
  )

  dataset[[new_var]] <- result
  dataset
}
