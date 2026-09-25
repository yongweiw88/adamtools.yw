#' Extract subject-level baseline values from a findings dataset
#'
#' For each requested output variable, extracts one value per subject from a
#' findings-type dataset (e.g. an SDTM domain or a BDS dataset), selecting
#' the baseline-flagged record when available and otherwise falling back to
#' the first record whose visit matches a supplied regular expression. This
#' generalizes the common "carry a baseline lab/vital value onto ADSL or
#' another dataset" pattern.
#'
#' @param dataset A data frame or tibble with one row per subject per test,
#'   containing `subject_var`, `testcd_var`, a value column, and `blfl_var`.
#' @param test_map A named character vector: names are the desired output
#'   variable names, values are the `testcd_var` codes to extract (e.g.
#'   `c(BWGTBL = "WEIGHT", BHGTBL = "HEIGHT")`).
#' @param subject_var Name of the subject identifier column. Defaults to
#'   `"USUBJID"`.
#' @param testcd_var Name of the test-code column used to look up `test_map`
#'   values. Defaults to `"TESTCD"`.
#' @param value_var Name of the column holding the value to extract (e.g.
#'   `"AVAL"` or `"STRESN"`). Defaults to `"AVAL"`.
#' @param blfl_var Name of the baseline-flag column. Records with this
#'   column equal to `"Y"` are preferred. Defaults to `"ABLFL"`.
#' @param visit_var Name of the visit column used for the fallback match
#'   when no baseline-flagged record exists. Defaults to `"VISIT"`.
#' @param fallback_visit_regex Optional regular expression (matched against
#'   `visit_var`, case-insensitive) identifying a fallback visit to use when
#'   no `blfl_var == "Y"` record exists for a subject/test (e.g.
#'   `"SCREEN"`). If `NULL` (default), no fallback is applied and subjects
#'   without a baseline-flagged record get `NA`.
#'
#' @return A data frame with one row per subject (`subject_var`) and one
#'   column per name in `test_map`.
#' @export
derive_subject_baseline_values <- function(dataset,
                                           test_map,
                                           subject_var = "USUBJID",
                                           testcd_var = "TESTCD",
                                           value_var = "AVAL",
                                           blfl_var = "ABLFL",
                                           visit_var = "VISIT",
                                           fallback_visit_regex = NULL) {
  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }
  required <- c(subject_var, testcd_var, value_var, blfl_var)
  missing_vars <- setdiff(required, names(dataset))
  if (length(missing_vars) > 0L) {
    stop(
      "`dataset` is missing required column(s): ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }
  if (is.null(names(test_map)) || any(names(test_map) == "")) {
    stop("`test_map` must be a named character vector.", call. = FALSE)
  }

  subjects <- unique(dataset[[subject_var]])
  result <- data.frame(subject_var = subjects, stringsAsFactors = FALSE)
  names(result) <- subject_var

  for (out_var in names(test_map)) {
    testcd <- test_map[[out_var]]
    subset_data <- dataset[dataset[[testcd_var]] == testcd, , drop = FALSE]

    baseline_recs <- subset_data[!is.na(subset_data[[blfl_var]]) & subset_data[[blfl_var]] == "Y", , drop = FALSE]

    fallback_recs <- NULL
    if (!is.null(fallback_visit_regex) && visit_var %in% names(subset_data)) {
      is_fallback <- grepl(fallback_visit_regex, subset_data[[visit_var]], ignore.case = TRUE)
      fallback_recs <- subset_data[is_fallback, , drop = FALSE]
    }

    values <- vapply(subjects, function(subj) {
      recs <- baseline_recs[baseline_recs[[subject_var]] == subj, , drop = FALSE]
      if (nrow(recs) == 0L && !is.null(fallback_recs)) {
        recs <- fallback_recs[fallback_recs[[subject_var]] == subj, , drop = FALSE]
      }
      if (nrow(recs) == 0L) return(NA_real_)
      as.numeric(recs[[value_var]][1])
    }, numeric(1))

    result[[out_var]] <- values
  }

  result
}
