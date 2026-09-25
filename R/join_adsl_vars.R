#' Join selected ADSL variables onto a dataset
#'
#' This helper is a lightweight wrapper around a left join that keeps the
#' selection of subject-level variables explicit and reusable. It is intended for
#' ADaM programs where the same ADSL variables are repeatedly merged onto a
#' subject-level analysis dataset.
#'
#' @param dataset A data frame or tibble to which ADSL variables should be added.
#' @param adsl ADSL dataset containing the subject-level variables to join.
#' @param vars Character vector of ADSL variable names to join. If `NULL`, all
#'   columns in `adsl` except the join keys are returned.
#' @param by Character vector of join keys. Defaults to `c("STUDYID", "USUBJID")`.
#'   If `STUDYID` is not present in `adsl`, it is ignored.
#' @param drop_subjid Logical flag to drop `SUBJID` from the automatically
#'   selected default variables. Defaults to `TRUE`.
#' @param overwrite_blank Logical flag controlling what happens when a
#'   requested ADSL variable already exists in `dataset` under the same
#'   name. Many SDTM parent domains (e.g. AE, CM, LB, EG, DV, MH, DD, IE,
#'   EX) carry their own `SUBJID` column that is entirely blank/`NA` in
#'   some study cuts -- previously this silently blocked the real,
#'   ADSL-derived `SUBJID` from ever being joined (with only a warning),
#'   which is almost never what's wanted. When `TRUE` (default), a
#'   same-named column in `dataset` that is entirely blank (`""`) or `NA`
#'   is treated as absent and overwritten by the ADSL value instead of
#'   being skipped; genuinely populated same-named columns are still left
#'   alone (skipped, with a warning) as before.
#'
#' @return A data frame with selected ADSL variables appended by subject.
#' @export
join_adsl_vars <- function(dataset,
                           adsl,
                           vars = NULL,
                           by = c("STUDYID", "USUBJID"),
                           drop_subjid = TRUE,
                           overwrite_blank = TRUE) {
  if (!inherits(dataset, "data.frame") || !inherits(adsl, "data.frame")) {
    stop("`dataset` and `adsl` must both be data frames or tibbles.")
  }

  by <- unique(by)
  missing_by <- setdiff(by, names(adsl))
  if (length(missing_by) > 0L) {
    by <- setdiff(by, missing_by)
  }

  if (length(by) == 0L) {
    stop("None of the requested join keys are present in `adsl`.")
  }

  if (!all(by %in% names(dataset))) {
    stop("All join keys in `by` must exist in `dataset`.")
  }

  if (is.null(vars)) {
    vars <- setdiff(names(adsl), by)
    if (drop_subjid && "SUBJID" %in% vars) {
      vars <- setdiff(vars, "SUBJID")
    }
  }

  vars <- unique(vars)
  vars <- intersect(vars, names(adsl))

  if (length(vars) == 0L) {
    return(dataset)
  }

  duplicate_names <- intersect(vars, names(dataset))
  is_blank <- function(x) all(is.na(x) | x == "")
  blank_names <- if (overwrite_blank) {
    duplicate_names[vapply(duplicate_names, function(v) is_blank(dataset[[v]]), logical(1))]
  } else {
    character(0)
  }
  skip_names <- setdiff(duplicate_names, blank_names)

  if (length(blank_names) > 0L) {
    dataset <- dataset[, setdiff(names(dataset), blank_names), drop = FALSE]
  }
  if (length(skip_names) > 0L) {
    vars <- setdiff(vars, skip_names)
    warning(
      "The following ADSL variables already existed in `dataset` (non-blank) and were not joined: ",
      paste(skip_names, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(vars) == 0L) {
    return(dataset)
  }

  adsl_subset <- adsl %>%
    dplyr::select(dplyr::all_of(c(by, vars)))

  dataset %>%
    dplyr::left_join(adsl_subset, by = by)
}
