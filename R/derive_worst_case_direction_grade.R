#' Derive 0-defaulted direction grades for worst-case (WOCGRL/WOCGRH) selection
#'
#' Companion to [derive_bidirectional_tox_grade()]. That function leaves a
#' Low/High-direction grade missing (`NA`) for a record that is evaluable
#' but not abnormal in that direction -- correct for per-record analysis
#' flags (e.g. `ANL03FL`), but it means a subject who is never abnormal in a
#' direction has no worst-case candidate record for that direction at all,
#' when a worst-case (`DTYPE = "WOCGRL"`/`"WOCGRH"`) derivation instead needs
#' a `GRADE 0` candidate for every graded record so a "worst" can still be
#' selected. This derives that 0-defaulted variant, for worst-case selection
#' only: `GRADE 0` when the record has a known overall grade (`graden`) but
#' is not abnormal in that direction, missing only when the overall grade
#' itself is unavailable. Gate on the overall grade, NOT on the
#' reference-range bound (`ANRLO`/`ANRHI`) being present -- a record's
#' overall grade can be known (e.g. `GRADE 0`) even when its reference range
#' happens to be missing for that specific record, and a historical
#' reference artifact confirmed worst-case candidacy follows the grade, not
#' the range. Use [derive_worst_case_grade_records()] on the resulting
#' columns to select the actual worst-case record(s); this function only
#' prepares the grade inputs.
#'
#' @param dataset A data frame or tibble, normally already processed by
#'   [derive_bidirectional_tox_grade()].
#' @param graden Name of the existing numeric overall grade column (e.g.
#'   `"ATOXGRN"`). Defaults to `"ATOXGRN"`.
#' @param lown Name of the existing numeric Low-direction grade column (from
#'   [derive_bidirectional_tox_grade()]). Defaults to `"ATOXGRLN"`.
#' @param highn Name of the existing numeric High-direction grade column
#'   (from [derive_bidirectional_tox_grade()]). Defaults to `"ATOXGRHN"`.
#' @param wc_lown Name of the new 0-defaulted Low-direction worst-case-
#'   selection column to create. Defaults to `"WC_ATOXGRLN"`.
#' @param wc_highn Name of the new 0-defaulted High-direction worst-case-
#'   selection column to create. Defaults to `"WC_ATOXGRHN"`.
#' @param overwrite Logical; allow overwriting `wc_lown`/`wc_highn` columns
#'   that already exist in `dataset`. Defaults to `FALSE`.
#'
#' @return `dataset` with the two 0-defaulted worst-case-selection columns
#'   added.
#' @export
derive_worst_case_direction_grade <- function(dataset,
                                              graden = "ATOXGRN",
                                              lown = "ATOXGRLN",
                                              highn = "ATOXGRHN",
                                              wc_lown = "WC_ATOXGRLN",
                                              wc_highn = "WC_ATOXGRHN",
                                              overwrite = FALSE) {
  .adamtools_check_data_frame(dataset)
  .adamtools_check_scalar_name(graden, "graden")
  .adamtools_check_scalar_name(lown, "lown")
  .adamtools_check_scalar_name(highn, "highn")
  .adamtools_check_scalar_name(wc_lown, "wc_lown")
  .adamtools_check_scalar_name(wc_highn, "wc_highn")
  .adamtools_check_columns(dataset, c(graden, lown, highn))
  .adamtools_check_output_columns(dataset, c(wc_lown, wc_highn), overwrite = overwrite)

  graden_v <- dataset[[graden]]
  lown_v <- dataset[[lown]]
  highn_v <- dataset[[highn]]

  result <- dataset
  result[[wc_lown]] <- ifelse(!is.na(graden_v), ifelse(is.na(lown_v), 0, lown_v), NA_real_)
  result[[wc_highn]] <- ifelse(!is.na(graden_v), ifelse(is.na(highn_v), 0, highn_v), NA_real_)
  result
}
