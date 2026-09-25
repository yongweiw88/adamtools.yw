#' Summarize source records to a subject-level positive/negative flag
#'
#' Creates one flag/status per subject (or per caller-supplied group) from
#' source-record evidence. The caller supplies study-agnostic logical
#' expressions for positive and optional negative evidence; the helper applies
#' the common precedence rule that any positive evidence wins, otherwise any
#' negative evidence returns the negative value, otherwise the missing value is
#' returned. It is intended for reusable mechanics such as baseline serology or
#' source-domain evidence summaries, not for hardcoding study-specific terms.
#'
#' @param dataset A data frame or tibble containing source records.
#' @param flag_var Name of the output flag/status variable.
#' @param positive Logical expression evaluated in `dataset` identifying
#'   positive evidence.
#' @param negative Optional logical expression evaluated in `dataset`
#'   identifying negative evidence. Defaults to `NULL`.
#' @param by Character vector of grouping variables. Defaults to `"USUBJID"`.
#' @param positive_value Value assigned when any positive evidence exists.
#'   Defaults to `"Y"`.
#' @param negative_value Value assigned when no positive evidence exists and
#'   any negative evidence exists. Defaults to `"N"`.
#' @param missing_value Value assigned when neither positive nor negative
#'   evidence exists. Defaults to `""`.
#' @param overwrite Logical; allow `flag_var` to replace an existing column in
#'   `dataset`. Defaults to `FALSE`.
#'
#' @return A data frame with one row per `by` group and `flag_var`.
#' @export
derive_subject_summary_flag <- function(dataset,
                                        flag_var,
                                        positive,
                                        negative = NULL,
                                        by = "USUBJID",
                                        positive_value = "Y",
                                        negative_value = "N",
                                        missing_value = "",
                                        overwrite = FALSE) {
  .adamtools_check_data_frame(dataset)
  .adamtools_check_scalar_name(flag_var, "flag_var")
  .adamtools_check_character_vector(by, "by", allow_null = FALSE)
  .adamtools_check_columns(dataset, by)
  .adamtools_check_output_columns(dataset, flag_var, overwrite = overwrite)

  positive_quo <- rlang::enquo(positive)
  negative_quo <- rlang::enquo(negative)

  pos <- rlang::eval_tidy(positive_quo, dataset)
  pos <- .adamtools_check_logical_condition(pos, nrow(dataset), "positive")

  if (rlang::quo_is_null(negative_quo)) {
    neg <- rep(FALSE, nrow(dataset))
  } else {
    neg <- rlang::eval_tidy(negative_quo, dataset)
    neg <- .adamtools_check_logical_condition(neg, nrow(dataset), "negative")
  }

  work <- dataset
  work$.adamtools_positive_ <- pos
  work$.adamtools_negative_ <- neg

  result <- dplyr::summarise(
    dplyr::group_by(work, dplyr::across(dplyr::all_of(by))),
    "{flag_var}" := dplyr::case_when(
      any(.data$.adamtools_positive_, na.rm = TRUE) ~ positive_value,
      any(.data$.adamtools_negative_, na.rm = TRUE) ~ negative_value,
      TRUE ~ missing_value
    ),
    .groups = "drop"
  )

  result
}

.adamtools_check_logical_condition <- function(x, n, arg) {
  if (!is.logical(x) || !(length(x) %in% c(1L, n))) {
    stop("`", arg, "` must evaluate to a logical vector of length 1 or nrow(dataset).", call. = FALSE)
  }
  if (length(x) == 1L) {
    x <- rep(x, n)
  }
  x[is.na(x)] <- FALSE
  x
}
