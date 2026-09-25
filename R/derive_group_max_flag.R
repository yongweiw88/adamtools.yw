#' Flag records at the group maximum of a numeric variable
#'
#' Adds a `"Y"`/`""` flag identifying every record whose value equals the
#' maximum of `value_var` within its `by_vars` group, and optionally
#' broadcasts that group maximum onto every row of the group. This
#' formalizes the repeated "compute the subject/parameter's maximum grade
#' and flag the record(s) that reached it" pattern (e.g. `MAXTOXFL`/
#' `MXTOXOFL`-style toxicity-grade flags), distinct from
#' [derive_worst_case_grade_records()]: that function selects/creates a
#' single new worst-case record, while this one annotates every existing
#' record in place (more than one record can tie for the group maximum and
#' all of them get flagged). Attaching a text label for the max value (e.g.
#' a graded-term lookup) is left to the caller via a follow-up join, the
#' same division of labor used by [derive_worst_case_grade_records()].
#'
#' @param dataset A data frame or tibble.
#' @param by_vars Character vector of grouping variables, e.g.
#'   `c("USUBJID", "AEDECOD", "AETERM")`.
#' @param value_var Name of the numeric column to find the group maximum
#'   of, e.g. `"ATOXGRN"`.
#' @param flag_var Name of the new flag column to create.
#' @param max_value_var Optional name of a new column to create holding the
#'   group's maximum value on every row of the group (e.g. `"MXTOXGRN"`).
#'   If `NULL` (default), only `flag_var` is added.
#' @param overwrite Logical; allow `flag_var`/`max_value_var` to replace
#'   existing columns in `dataset`. Defaults to `FALSE`.
#'
#' @return `dataset` with `flag_var` (and optionally `max_value_var`)
#'   added. Rows with a missing `value_var` are never flagged and get `""`;
#'   groups where every `value_var` is missing get a missing `max_value_var`.
#' @export
derive_group_max_flag <- function(dataset,
                                  by_vars,
                                  value_var,
                                  flag_var,
                                  max_value_var = NULL,
                                  overwrite = FALSE) {
  .adamtools_check_data_frame(dataset)
  .adamtools_check_character_vector(by_vars, "by_vars", allow_null = FALSE)
  .adamtools_check_scalar_name(value_var, "value_var")
  .adamtools_check_scalar_name(flag_var, "flag_var")
  .adamtools_check_scalar_name(max_value_var, "max_value_var", allow_null = TRUE)
  .adamtools_check_columns(dataset, unique(c(by_vars, value_var)))
  .adamtools_check_output_columns(dataset, c(flag_var, max_value_var), overwrite = overwrite)
  .adamtools_check_internal_columns(dataset, ".adamtools_group_max_")

  eligible <- dataset[!is.na(dataset[[value_var]]), , drop = FALSE]
  max_by_group <- eligible %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by_vars))) %>%
    dplyr::summarise(.adamtools_group_max_ = max(.data[[value_var]]), .groups = "drop")

  result <- dplyr::left_join(dataset, max_by_group, by = by_vars)
  result[[flag_var]] <- ifelse(
    !is.na(result[[value_var]]) & !is.na(result$.adamtools_group_max_) &
      result[[value_var]] == result$.adamtools_group_max_,
    "Y", ""
  )
  if (!is.null(max_value_var)) {
    result[[max_value_var]] <- result$.adamtools_group_max_
  }
  result$.adamtools_group_max_ <- NULL

  result
}
