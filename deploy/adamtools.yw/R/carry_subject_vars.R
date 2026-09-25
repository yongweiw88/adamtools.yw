#' Carry forward variables that are constant within a subject
#'
#' For each requested variable, collapses multiple per-subject records to a
#' single value only when all non-missing values within the subject agree;
#' if a subject has conflicting non-missing values for a variable, the
#' result is `NA` for that subject/variable (rather than silently picking
#' one). This is intended for building/augmenting ADSL-style one-row-per-
#' subject datasets from a multi-record source (e.g. carrying a
#' subject-level flag or covariate captured redundantly across visits).
#'
#' @param dataset A data frame or tibble with one or more rows per subject.
#' @param subject_var Name of the subject identifier column. Defaults to
#'   `"USUBJID"`.
#' @param vars Character vector of column names in `dataset` to carry
#'   forward.
#'
#' @return A data frame with one row per distinct `subject_var` value and
#'   one column per entry in `vars`.
#' @export
carry_subject_vars <- function(dataset, subject_var = "USUBJID", vars) {
  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }
  if (!subject_var %in% names(dataset)) {
    stop("`subject_var` is not a column in `dataset`.", call. = FALSE)
  }
  missing_vars <- setdiff(vars, names(dataset))
  if (length(missing_vars) > 0L) {
    stop(
      "`vars` refers to column(s) not found in `dataset`: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }

  subjects <- unique(dataset[[subject_var]])
  result <- data.frame(subject_var = subjects, stringsAsFactors = FALSE)
  names(result) <- subject_var

  carry_one <- function(x) {
    na_val <- x[NA_integer_]
    non_na <- x[!is.na(x)]
    unique_vals <- unique(non_na)
    if (length(unique_vals) != 1L) return(na_val)
    unique_vals[1]
  }

  for (v in vars) {
    col <- dataset[[v]]
    values_by_subj <- split(col, dataset[[subject_var]])[as.character(subjects)]
    result[[v]] <- unlist(lapply(values_by_subj, carry_one), use.names = FALSE)
  }

  result
}
