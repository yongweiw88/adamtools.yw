#' Derive worst-case toxicity/abnormality grade records (DTYPE-flagged)
#'
#' Derives the additional analysis records commonly required for lab/vitals/
#' ECG BDS datasets that summarize the single "worst" post-baseline record per
#' subject and parameter (e.g. `DTYPE = "WOC"`, `"WOCGR"`, `"WOCGRH"`,
#' `"WOCGRL"`). This formalizes the repeated "filter to qualifying records,
#' order by grade magnitude/direction plus tie-break columns, keep exactly
#' one record" pattern without hardcoding any study-specific grading scale,
#' visit windows, or qualifying conditions. It composes this package's
#' [select_subject_extreme_record()] rather than reimplementing selection
#' logic, and returns only the newly derived worst-case record(s) so the
#' caller can `dplyr::bind_rows()` them onto the parent BDS dataset (the same
#' composition style as `admiral::derive_extreme_records()`).
#'
#' Call this function once per required `DTYPE` value. For example, a study
#' program deriving `WOC` (worst overall grade, either direction), `WOCGRH`
#' (worst high/abnormal-high grade), and `WOCGRL` (worst low/abnormal-low
#' grade) would call it three times with `direction = "abs"`, `"high"`, and
#' `"low"` respectively, each with the study-specific `condition` that
#' defines "qualifying" (e.g. on-treatment, non-missing grade, excluding
#' unscheduled visits) supplied by the caller.
#'
#' @param dataset A data frame or tibble of candidate records (already
#'   restricted to the population the worst-case record should be selected
#'   from, e.g. post-baseline/on-treatment records, in addition to any
#'   `condition`).
#' @param by_vars Character vector of grouping variables identifying one
#'   worst-case record per group, e.g. `c("USUBJID", "PARAMCD")`.
#' @param grade_var Name of the numeric grade/toxicity variable used to
#'   determine the worst record, e.g. `"ATOXGR"`.
#' @param dtype Single character value assigned to the new `DTYPE` variable
#'   for the derived record(s), e.g. `"WOC"`, `"WOCGR"`, `"WOCGRH"`,
#'   `"WOCGRL"`.
#' @param condition Logical vector (same length as `nrow(dataset)`) or a
#'   single logical value giving the study-specific eligibility condition a
#'   record must meet to be a worst-case candidate (e.g. on-treatment flag,
#'   direction-specific grade sign). Defaults to `TRUE` (all records with a
#'   non-missing `grade_var` are eligible).
#' @param direction One of `"high"` (largest signed `grade_var` wins, e.g.
#'   worst high-abnormality grade), `"low"` (smallest/most negative signed
#'   `grade_var` wins, e.g. worst low-abnormality grade), or `"abs"` (largest
#'   absolute `grade_var` wins, e.g. worst grade in either direction).
#'   Defaults to `"abs"`.
#' @param tie_break Optional character vector of additional ordering
#'   variables (e.g. `c("ADT", "LBSEQ")`) used to break ties when more than
#'   one record in a group shares the same (signed or absolute) grade value.
#'   Passed through to [select_subject_extreme_record()]. Records ties as an
#'   explicit error unless `ties` is set, so the study program must supply
#'   enough tie-break columns to be deterministic.
#' @param ties Handling when a group still has more than one qualifying
#'   record after `grade_var` and `tie_break` ordering: `"error"` (default),
#'   `"first"`, or `"last"`. Passed through to
#'   [select_subject_extreme_record()].
#' @param keep Optional character vector of columns to keep in the returned
#'   worst-case record(s), in addition to `by_vars`. If `NULL` (default), all
#'   columns from `dataset` are kept.
#'
#' @return A data frame containing at most one derived worst-case record per
#'   `by_vars` group, with `DTYPE` set to `dtype`. Does **not** include the
#'   original (non-worst-case) records; combine with `dplyr::bind_rows()`.
#' @export
derive_worst_case_grade_records <- function(dataset,
                                            by_vars,
                                            grade_var,
                                            dtype,
                                            condition = TRUE,
                                            direction = c("abs", "high", "low"),
                                            tie_break = NULL,
                                            ties = c("error", "first", "last"),
                                            keep = NULL) {
  .adamtools_check_data_frame(dataset)
  .adamtools_check_character_vector(by_vars, "by_vars", allow_null = FALSE)
  .adamtools_check_scalar_name(grade_var, "grade_var")
  .adamtools_check_columns(dataset, unique(c(by_vars, grade_var, tie_break)))
  if (!is.character(dtype) || length(dtype) != 1L || is.na(dtype) || identical(dtype, "")) {
    stop("`dtype` must be a single non-missing character value.", call. = FALSE)
  }
  direction <- match.arg(direction)
  ties <- match.arg(ties)
  .adamtools_check_internal_columns(dataset, ".adamtools_abs_grade_")

  eligible <- condition & !is.na(dataset[[grade_var]])
  if (length(eligible) == 1L) {
    eligible <- rep(eligible, nrow(dataset))
  }
  candidates <- dataset[eligible, , drop = FALSE]

  if (nrow(candidates) == 0L) {
    output_cols <- if (is.null(keep)) names(dataset) else unique(c(by_vars, keep))
    result <- candidates[, output_cols, drop = FALSE]
    result$DTYPE <- character(0)
    return(result)
  }

  order_vars <- switch(direction,
    abs = {
      candidates$.adamtools_abs_grade_ <- abs(candidates[[grade_var]])
      c(".adamtools_abs_grade_", tie_break)
    },
    high = c(grade_var, tie_break),
    low = c(grade_var, tie_break)
  )
  mode <- if (identical(direction, "low")) "first" else "last"

  keep_cols <- if (is.null(keep)) NULL else unique(c(keep, grade_var))
  selected <- select_subject_extreme_record(
    candidates,
    by = by_vars,
    order = order_vars,
    mode = mode,
    keep = keep_cols,
    ties = ties
  )

  selected$.adamtools_abs_grade_ <- NULL
  selected$DTYPE <- dtype
  selected
}
