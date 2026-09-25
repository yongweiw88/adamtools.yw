#' Find subjects with two qualifying events within N days of each other
#'
#' Identifies subjects present in both of two event-date data frames whose
#' dates fall within a caller-specified window of each other. This
#' formalizes the repeated "criterion A occurred, criterion B occurred
#' within N days of it" composite-criteria pattern seen in multi-parameter
#' safety signals such as Hy's Law (e.g. ALT elevation and bilirubin
#' elevation within 28 days), without hardcoding any study-specific
#' criteria, parameters, or window length.
#'
#' @param event1,event2 Data frames of qualifying event dates, each
#'   containing `by_vars` and `date_var` (e.g. one row per subject per
#'   qualifying record, already filtered to the criterion of interest --
#'   the same shape as the `condition`-filtered inputs used elsewhere in
#'   this package, such as [derive_worst_case_grade_records()]).
#' @param by_vars Character vector of subject/grouping key columns present
#'   in both `event1` and `event2`. Defaults to `"USUBJID"`.
#' @param date_var Name of the date column present in both `event1` and
#'   `event2`. Defaults to `"ADT"`.
#' @param within_days Maximum absolute number of days apart the two events
#'   may be and still qualify as "paired". Defaults to `28`.
#'
#' @return A data frame with one row per qualifying `by_vars` combination
#'   (subjects/groups with at least one `event1`/`event2` date pair within
#'   `within_days` of each other). Does not return the qualifying dates
#'   themselves -- join back to `event1`/`event2` if the specific date is
#'   needed.
#' @export
derive_paired_event_subjects <- function(event1,
                                         event2,
                                         by_vars = "USUBJID",
                                         date_var = "ADT",
                                         within_days = 28) {
  .adamtools_check_data_frame(event1)
  .adamtools_check_data_frame(event2)
  .adamtools_check_character_vector(by_vars, "by_vars", allow_null = FALSE)
  .adamtools_check_scalar_name(date_var, "date_var")
  .adamtools_check_columns(event1, c(by_vars, date_var))
  .adamtools_check_columns(event2, c(by_vars, date_var))

  if (nrow(event1) == 0L || nrow(event2) == 0L) {
    return(event1[FALSE, by_vars, drop = FALSE])
  }

  date1 <- paste0(date_var, "_1")
  date2 <- paste0(date_var, "_2")

  paired <- dplyr::inner_join(
    event1, event2,
    by = by_vars, suffix = c("_1", "_2"), relationship = "many-to-many"
  )
  paired <- paired[abs(as.numeric(paired[[date1]] - paired[[date2]])) <= within_days, , drop = FALSE]

  dplyr::distinct(paired, dplyr::across(dplyr::all_of(by_vars)))
}
