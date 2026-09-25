#' Derive subject-level milestone dates from a Disposition/Events domain
#'
#' Extracts the earliest date per subject matching each of one or more
#' named milestone criteria (e.g. randomization, screen failure, informed
#' consent) from an SDTM disposition/events-style domain. Generalizes the
#' repeated "scan DS for a matching DSDECOD/DSTERM value and take its date"
#' pattern to an arbitrary named list of criteria, so milestone names are
#' never hardcoded in derivation code.
#'
#' @param dataset An SDTM domain data frame (e.g. `DS`) containing
#'   `subject_var`, `criteria_var`, and `date_var`.
#' @param milestones A named list of character vectors: each name is the
#'   output date-variable name (e.g. `"RANDDT"`), and each value is a
#'   character vector of `criteria_var` values that qualify for that
#'   milestone (matched case-insensitively), e.g. `list(RANDDT =
#'   "RANDOMIZED", SCRFDT = c("SCREEN FAILURE", "SCREEN FAILED"))`.
#' @param subject_var Name of the subject identifier column. Defaults to
#'   `"USUBJID"`.
#' @param criteria_var Name of the column to match milestone criteria
#'   against (e.g. `"DSDECOD"`). Defaults to `"DSDECOD"`.
#' @param date_var Name of the date column to extract (e.g. `"DSSTDTC"`).
#'   Defaults to `"DSSTDTC"`.
#' @param date_imputation Passed to `admiral::convert_dtc_to_dt()` as
#'   `highest_imputation`; defaults to `"n"` (no imputation).
#'
#' @return A data frame with one row per distinct `subject_var` value and
#'   one `Date` column per name in `milestones`.
#' @export
derive_subject_milestone_dates <- function(dataset,
                                           milestones,
                                           subject_var = "USUBJID",
                                           criteria_var = "DSDECOD",
                                           date_var = "DSSTDTC",
                                           date_imputation = "n") {
  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }
  required <- c(subject_var, criteria_var, date_var)
  missing_vars <- setdiff(required, names(dataset))
  if (length(missing_vars) > 0L) {
    stop(
      "`dataset` is missing required column(s): ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }
  if (is.null(names(milestones)) || any(names(milestones) == "")) {
    stop("`milestones` must be a named list.", call. = FALSE)
  }

  subjects <- unique(dataset[[subject_var]])
  result <- data.frame(subject_var = subjects, stringsAsFactors = FALSE)
  names(result) <- subject_var

  crit_vals <- toupper(trimws(as.character(dataset[[criteria_var]])))
  parsed_dates <- admiral::convert_dtc_to_dt(dataset[[date_var]], highest_imputation = date_imputation)

  for (out_var in names(milestones)) {
    crit_set <- toupper(trimws(milestones[[out_var]]))
    matches <- crit_vals %in% crit_set

    subj_dates <- rep(as.Date(NA), length(subjects))
    names(subj_dates) <- as.character(subjects)
    match_subjects <- dataset[[subject_var]][matches]
    match_dates <- parsed_dates[matches]
    for (i in seq_along(match_subjects)) {
      s <- as.character(match_subjects[i])
      d <- match_dates[i]
      if (is.na(d)) next
      if (is.na(subj_dates[[s]]) || d < subj_dates[[s]]) {
        subj_dates[[s]] <- d
      }
    }
    result[[out_var]] <- subj_dates[as.character(subjects)]
  }

  result
}
