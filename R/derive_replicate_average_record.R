#' Derive a mean-of-replicates record (DTYPE-flagged)
#'
#' Derives one additional analysis record per group that averages a numeric
#' value across repeated same-occasion measurements (e.g. triplicate ECG
#' readings, replicate vital signs), when at least `min_replicates` non-
#' missing values exist in the group. This formalizes the "filter to
#' qualifying records, average the value, keep exactly one record"
#' pattern without hardcoding any study-specific replicate structure,
#' analogous to how [derive_worst_case_grade_records()] formalizes
#' worst-case selection instead of averaging. It composes this package's
#' [select_subject_extreme_record()] to pick a deterministic representative
#' record for the non-averaged columns, and returns only the newly derived
#' average record(s) so the caller can `dplyr::bind_rows()` them onto the
#' parent BDS dataset.
#'
#' @param dataset A data frame or tibble of candidate records (already
#'   restricted to the population an average record should be derived from,
#'   in addition to any `condition`).
#' @param by_vars Character vector of grouping variables identifying one
#'   replicate occasion, e.g. `c("USUBJID", "PARAMCD", "VISITNUM", "ATPTGRP")`.
#' @param aval_var Name of the numeric value variable to average, e.g.
#'   `"AVAL"`. Defaults to `"AVAL"`.
#' @param dtype Single character value assigned to the new `DTYPE` variable
#'   for the derived record(s), e.g. `"AVERAGE"`.
#' @param condition Logical vector (same length as `nrow(dataset)`) or a
#'   single logical value giving the study-specific eligibility condition a
#'   record must meet to be an averaging candidate. Defaults to `TRUE` (all
#'   records with a non-missing `aval_var` are eligible).
#' @param min_replicates Minimum number of non-missing `aval_var` values
#'   required within a group for an average record to be derived. Groups
#'   with fewer eligible records are dropped (the caller's existing single
#'   record already represents that occasion; no average is created).
#'   Defaults to `2`.
#' @param order_vars Optional character vector of ordering variables (e.g.
#'   `c("ADT")`) used to pick which group record is the representative for
#'   every column other than `aval_var` (e.g. dates, labels, IDs) -- the
#'   last record by `order_vars` is used, matching
#'   [select_subject_extreme_record()]'s `mode = "last"`. If `NULL`
#'   (default), an arbitrary-but-deterministic record (input row order) is
#'   used, which is safe when the non-averaged columns are already
#'   record-invariant within the group (e.g. `AVISIT`/`PARAM`).
#' @param keep Optional character vector of columns to keep in the returned
#'   average record(s), in addition to `by_vars`. If `NULL` (default), all
#'   columns from `dataset` are kept.
#' @param ties Handling when `order_vars` (or the fallback row-order key)
#'   still leaves more than one candidate representative record: `"error"`
#'   (default), `"first"`, or `"last"`. Passed through to
#'   [select_subject_extreme_record()].
#'
#' @return A data frame containing at most one derived average record per
#'   `by_vars` group, with `DTYPE` set to `dtype` and `aval_var` set to the
#'   group's mean. Does **not** include the original (non-averaged) records;
#'   combine with `dplyr::bind_rows()`.
#' @export
derive_replicate_average_record <- function(dataset,
                                            by_vars,
                                            aval_var = "AVAL",
                                            dtype,
                                            condition = TRUE,
                                            min_replicates = 2,
                                            order_vars = NULL,
                                            keep = NULL,
                                            ties = c("error", "first", "last")) {
  .adamtools_check_data_frame(dataset)
  .adamtools_check_character_vector(by_vars, "by_vars", allow_null = FALSE)
  .adamtools_check_scalar_name(aval_var, "aval_var")
  .adamtools_check_character_vector(order_vars, "order_vars", allow_null = TRUE)
  .adamtools_check_columns(dataset, unique(c(by_vars, aval_var, order_vars)))
  if (!is.character(dtype) || length(dtype) != 1L || is.na(dtype) || identical(dtype, "")) {
    stop("`dtype` must be a single non-missing character value.", call. = FALSE)
  }
  ties <- match.arg(ties)
  .adamtools_check_internal_columns(dataset, c(".adamtools_group_n_", ".adamtools_row_", ".adamtools_mean_"))

  eligible <- condition & !is.na(dataset[[aval_var]])
  if (length(eligible) == 1L) {
    eligible <- rep(eligible, nrow(dataset))
  }
  candidates <- dataset[eligible, , drop = FALSE]

  output_cols <- if (is.null(keep)) names(dataset) else unique(c(by_vars, keep))
  empty_result <- function(d) {
    result <- d[, intersect(output_cols, names(d)), drop = FALSE]
    result$DTYPE <- character(0)
    result
  }
  if (nrow(candidates) == 0L) {
    return(empty_result(candidates))
  }

  candidates <- candidates %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by_vars))) %>%
    dplyr::mutate(.adamtools_group_n_ = dplyr::n()) %>%
    dplyr::ungroup()

  eligible_groups <- candidates[candidates$.adamtools_group_n_ >= min_replicates, , drop = FALSE]
  if (nrow(eligible_groups) == 0L) {
    return(empty_result(eligible_groups))
  }

  means <- eligible_groups %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by_vars))) %>%
    dplyr::summarise(.adamtools_mean_ = mean(.data[[aval_var]]), .groups = "drop")

  eligible_groups$.adamtools_row_ <- seq_len(nrow(eligible_groups))
  representative <- select_subject_extreme_record(
    eligible_groups,
    by = by_vars,
    order = if (is.null(order_vars)) ".adamtools_row_" else order_vars,
    mode = "last",
    keep = if (is.null(keep)) NULL else unique(c(keep, ".adamtools_row_")),
    ties = ties
  )

  result <- representative %>%
    dplyr::left_join(means, by = by_vars) %>%
    dplyr::mutate(!!aval_var := .data$.adamtools_mean_, DTYPE = dtype)
  result$.adamtools_mean_ <- NULL
  result$.adamtools_row_ <- NULL
  result$.adamtools_group_n_ <- NULL

  result
}
