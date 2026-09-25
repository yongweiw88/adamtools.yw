#' Split an overall toxicity grade into bi-directional Low/High components
#'
#' Derives the Low- and High-direction toxicity grade variables (e.g.
#' `ATOXGRL`/`ATOXGRLN`, `ATOXGRH`/`ATOXGRHN`, or their baseline counterparts
#' `BTOXGRL`/`BTOXGRH`) from an already-derived overall grade (the source lab
#' data is assumed already graded, e.g. via `LBTOXGR`; this does not re-derive
#' the grade itself), by comparing the analysis value to the analysis
#' reference range. This formalizes the ADaM BDS convention that a graded
#' parameter's grade is attributed to the Low direction only when the value
#' falls below the reference range lower limit (`AVAL < ANRLO`) and to the
#' High direction only when it falls above the reference range upper limit
#' (`AVAL > ANRHI`). Parameters whose value is inside the reference range
#' (e.g. Grade 0) or whose reference range is not evaluable get neither, so
#' they are picked up by an undirected/single-scale worst-case category (e.g.
#' `DTYPE = "WOCGR"`) rather than a Low/High split (`WOCGRL`/`WOCGRH`)
#' downstream. No study-specific bi-directional-parameter list is required:
#' the direction split falls out of the value/reference-range comparison per
#' record.
#'
#' @param dataset A data frame or tibble.
#' @param aval Name of the numeric analysis value column. Defaults to `"AVAL"`.
#' @param anrlo Name of the numeric analysis reference range lower limit
#'   column. Defaults to `"ANRLO"`.
#' @param anrhi Name of the numeric analysis reference range upper limit
#'   column. Defaults to `"ANRHI"`.
#' @param grade Name of the existing character overall grade column (e.g.
#'   `"ATOXGR"`). Defaults to `"ATOXGR"`.
#' @param graden Name of the existing numeric overall grade column (e.g.
#'   `"ATOXGRN"`). Defaults to `"ATOXGRN"`.
#' @param low Name of the new character Low-direction grade column to
#'   create. Defaults to `"ATOXGRL"`.
#' @param lown Name of the new numeric Low-direction grade column to create.
#'   Defaults to `"ATOXGRLN"`.
#' @param high Name of the new character High-direction grade column to
#'   create. Defaults to `"ATOXGRH"`.
#' @param highn Name of the new numeric High-direction grade column to
#'   create. Defaults to `"ATOXGRHN"`.
#' @param overwrite Logical; allow overwriting `low`/`lown`/`high`/`highn`
#'   columns that already exist in `dataset`. Defaults to `FALSE`.
#'
#' @return `dataset` with the Low/High-direction grade columns added.
#' @export
derive_bidirectional_tox_grade <- function(dataset,
                                           aval = "AVAL",
                                           anrlo = "ANRLO",
                                           anrhi = "ANRHI",
                                           grade = "ATOXGR",
                                           graden = "ATOXGRN",
                                           low = "ATOXGRL",
                                           lown = "ATOXGRLN",
                                           high = "ATOXGRH",
                                           highn = "ATOXGRHN",
                                           overwrite = FALSE) {
  .adamtools_check_data_frame(dataset)
  .adamtools_check_scalar_name(aval, "aval")
  .adamtools_check_scalar_name(anrlo, "anrlo")
  .adamtools_check_scalar_name(anrhi, "anrhi")
  .adamtools_check_scalar_name(grade, "grade")
  .adamtools_check_scalar_name(graden, "graden")
  .adamtools_check_scalar_name(low, "low")
  .adamtools_check_scalar_name(lown, "lown")
  .adamtools_check_scalar_name(high, "high")
  .adamtools_check_scalar_name(highn, "highn")
  .adamtools_check_columns(dataset, c(aval, anrlo, anrhi, grade, graden))
  .adamtools_check_output_columns(dataset, c(low, lown, high, highn), overwrite = overwrite)

  aval_v <- dataset[[aval]]
  anrlo_v <- dataset[[anrlo]]
  anrhi_v <- dataset[[anrhi]]
  grade_v <- dataset[[grade]]
  graden_v <- dataset[[graden]]

  is_low <- !is.na(graden_v) & !is.na(aval_v) & !is.na(anrlo_v) & aval_v < anrlo_v
  is_high <- !is.na(graden_v) & !is.na(aval_v) & !is.na(anrhi_v) & aval_v > anrhi_v

  result <- dataset
  result[[low]] <- ifelse(is_low, grade_v, "")
  result[[lown]] <- ifelse(is_low, graden_v, NA_real_)
  result[[high]] <- ifelse(is_high, grade_v, "")
  result[[highn]] <- ifelse(is_high, graden_v, NA_real_)
  result
}
