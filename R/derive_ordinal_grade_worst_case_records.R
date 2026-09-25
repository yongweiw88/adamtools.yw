#' Derive independent ordinal-scale and toxicity-grade worst-case records
#'
#' Some ADaM BDS lab-type datasets need TWO independent "worst case"
#' summaries per group (e.g. per `USUBJID`/`PARAMCD`): one on a qualitative/
#' ordinal result scale (e.g. a urinalysis dipstick NEGATIVE/TRACE/1+/2+/3+/
#' 4+ scale), and one on the overall toxicity grade (e.g. `ATOXGRN`, only
#' populated when the source grading variable, e.g. `LBTOXGR`, was actually
#' assigned for that record). These two worst-case records are **not**
#' mutually exclusive: the same record can be selected as the "worst" for
#' both simultaneously (e.g. a record with an ordinal value that also
#' happens to carry a real overall grade). Do not gate one on the presence
#' or absence of the other -- compute them independently and combine.
#'
#' This composes two independent [derive_worst_case_grade_records()] calls
#' (one per scale, `direction = "high"` for both) and binds the results,
#' so callers don't have to hand-write two near-identical calls or
#' second-guess whether the two scales should be mutually exclusive.
#'
#' @param dataset A data frame or tibble of candidate records (already
#'   restricted to the population the worst-case records should be selected
#'   from, e.g. post-baseline records).
#' @param by_vars Character vector of grouping variables, e.g.
#'   `c("USUBJID", "PARAMCD")`.
#' @param ordinal_var Name of the numeric ordinal-scale variable (e.g.
#'   `"AVALCN"`).
#' @param ordinal_dtype Single character `DTYPE` value for the ordinal
#'   worst-case record (e.g. `"WOC"`).
#' @param ordinal_condition Logical vector or single logical value
#'   restricting which records are eligible ordinal-scale candidates (e.g.
#'   `PARCAT1 == "URINALYSIS"`). Defaults to `TRUE` (all records with a
#'   non-missing `ordinal_var`).
#' @param grade_var Name of the numeric overall-grade variable (e.g.
#'   `"ATOXGRN"`).
#' @param grade_dtype Single character `DTYPE` value for the grade
#'   worst-case record (e.g. `"WOCGR"`).
#' @param grade_condition Logical vector or single logical value restricting
#'   which records are eligible grade candidates. Defaults to `TRUE` (all
#'   records with a non-missing `grade_var`).
#' @param tie_break Optional character vector of additional ordering
#'   variables used to break ties within each scale. Passed through to both
#'   underlying calls.
#' @param ties Tie handling passed through to both underlying calls:
#'   `"error"` (default), `"first"`, or `"last"`.
#' @param keep Optional character vector of columns to keep in the returned
#'   record(s), in addition to `by_vars`. Passed through to both underlying
#'   calls.
#'
#' @return A data frame with 0, 1, or 2 derived worst-case records per
#'   `by_vars` group (0 if neither scale has an eligible candidate, 2 if
#'   both do -- including when both select the same underlying record).
#'   Does **not** include the original (non-worst-case) records; combine
#'   with `dplyr::bind_rows()` onto the parent BDS dataset.
#' @export
derive_ordinal_grade_worst_case_records <- function(dataset,
                                                     by_vars,
                                                     ordinal_var,
                                                     ordinal_dtype,
                                                     ordinal_condition = TRUE,
                                                     grade_var,
                                                     grade_dtype,
                                                     grade_condition = TRUE,
                                                     tie_break = NULL,
                                                     ties = c("error", "first", "last"),
                                                     keep = NULL) {
  ties <- match.arg(ties)

  ordinal_records <- derive_worst_case_grade_records(
    dataset,
    by_vars = by_vars,
    grade_var = ordinal_var,
    dtype = ordinal_dtype,
    condition = ordinal_condition,
    direction = "high",
    tie_break = tie_break,
    ties = ties,
    keep = keep
  )

  grade_records <- derive_worst_case_grade_records(
    dataset,
    by_vars = by_vars,
    grade_var = grade_var,
    dtype = grade_dtype,
    condition = grade_condition,
    direction = "high",
    tie_break = tie_break,
    ties = ties,
    keep = keep
  )

  dplyr::bind_rows(ordinal_records, grade_records)
}
