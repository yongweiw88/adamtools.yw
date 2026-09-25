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
#'
#' @return A data frame with selected ADSL variables appended by subject.
#' @export
join_adsl_vars <- function(dataset,
                           adsl,
                           vars = NULL,
                           by = c("STUDYID", "USUBJID"),
                           drop_subjid = TRUE) {
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
  if (length(duplicate_names) > 0L) {
    vars <- setdiff(vars, duplicate_names)
    warning(
      "The following ADSL variables already existed in `dataset` and were not joined: " ,
      paste(duplicate_names, collapse = ", "),
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
