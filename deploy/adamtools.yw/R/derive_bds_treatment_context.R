#' Derive BDS treatment/period/phase context from ADSL period boundaries
#'
#' Derives `APERIOD`/`APERIODC`, a pre-/on-treatment `APHASE`, and per-period
#' `TRTP`/`TRTA` (character) context on a BDS-style analysis dataset, using a
#' caller-supplied period-boundary map instead of hardcoded study period
#' labels or dates. This generalizes the repeated "assign observation-level
#' period/phase/treatment context from ADSL period variables" pattern (see
#' also the crossover `TRTA`/`TRTP` period-assignment guidance in the
#' `adam-function-developer`/`adam-r-package-engineer` agent files).
#'
#' @param dataset A data frame or tibble containing `date_var`,
#'   `trt_start_var`, and the period start-date columns referenced in
#'   `period_map`.
#' @param date_var Name of the observation date variable (e.g. `"ADT"`) used
#'   to determine period/phase. Defaults to `"ADT"`.
#' @param trt_start_var Name of the overall treatment start date variable
#'   (e.g. `"TRTSDT"`). Observations before this date are treated as
#'   pre-treatment (no period assigned). Defaults to `"TRTSDT"`.
#' @param period_map A data frame with one row per period, containing
#'   columns `period` (numeric period number), `start_var` (character; name
#'   of the column in `dataset` holding that period's start date), and
#'   optionally `label` (character; the `APERIODC` value for that period).
#'   If `NULL` (default), no period/`APERIOD` derivation is performed.
#' @param period_var Name of the new numeric period variable. Defaults to
#'   `"APERIOD"`.
#' @param periodc_var Name of the new character period-label variable.
#'   Defaults to `"APERIODC"`.
#' @param phase_var Name of the new phase variable. Defaults to `"APHASE"`.
#' @param phase_labels Named character vector with elements `"pre"` and
#'   `"on"` giving the phase labels for pre-treatment and on-treatment
#'   observations. Defaults to `c(pre = "Pre-Treatment", on = "On-Treatment")`.
#' @param trtp_map Optional named character vector mapping period number
#'   (as a string, e.g. `"1"`, `"2"`) to the column in `dataset` holding the
#'   planned treatment for that period (e.g. `c("1" = "TRT01P", "2" =
#'   "TRT02P")`), used to derive a period-aware `TRTP`.
#' @param trta_map Optional named character vector mapping period number
#'   (as a string) to the column in `dataset` holding the actual treatment
#'   for that period (e.g. `c("1" = "TRT01A", "2" = "TRT02A")`), used to
#'   derive a period-aware `TRTA`.
#' @param trtp_var Name of the new period-aware planned-treatment variable.
#'   Defaults to `"TRTP"`.
#' @param trta_var Name of the new period-aware actual-treatment variable.
#'   Defaults to `"TRTA"`.
#'
#' @return `dataset` with the requested period/phase/treatment context
#'   variables added.
#' @export
derive_bds_treatment_context <- function(dataset,
                                         date_var = "ADT",
                                         trt_start_var = "TRTSDT",
                                         period_map = NULL,
                                         period_var = "APERIOD",
                                         periodc_var = "APERIODC",
                                         phase_var = "APHASE",
                                         phase_labels = c(pre = "Pre-Treatment", on = "On-Treatment"),
                                         trtp_map = NULL,
                                         trta_map = NULL,
                                         trtp_var = "TRTP",
                                         trta_var = "TRTA") {
  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }
  if (!date_var %in% names(dataset)) {
    stop("`date_var` is not a column in `dataset`.", call. = FALSE)
  }
  if (!trt_start_var %in% names(dataset)) {
    stop("`trt_start_var` is not a column in `dataset`.", call. = FALSE)
  }

  result <- dataset
  date_vals <- result[[date_var]]
  trt_start_vals <- result[[trt_start_var]]
  is_pre <- !is.na(date_vals) & !is.na(trt_start_vals) & date_vals < trt_start_vals

  result[[phase_var]] <- ifelse(is.na(date_vals) | is.na(trt_start_vals), NA_character_,
                                ifelse(is_pre, phase_labels[["pre"]], phase_labels[["on"]]))

  period_assigned <- rep(NA_real_, nrow(result))
  if (!is.null(period_map)) {
    required_cols <- c("period", "start_var")
    missing_cols <- setdiff(required_cols, names(period_map))
    if (length(missing_cols) > 0L) {
      stop(
        "`period_map` is missing required column(s): ",
        paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }
    period_map <- period_map[order(period_map$period), , drop = FALSE]
    for (i in seq_len(nrow(period_map))) {
      p <- period_map$period[i]
      start_col <- period_map$start_var[i]
      if (!start_col %in% names(result)) next
      start_vals <- result[[start_col]]
      qualifies <- !is.na(date_vals) & !is.na(start_vals) & date_vals >= start_vals & !is_pre
      period_assigned[qualifies] <- p
    }
    result[[period_var]] <- period_assigned

    if ("label" %in% names(period_map)) {
      label_lookup <- stats::setNames(period_map$label, period_map$period)
      result[[periodc_var]] <- unname(label_lookup[as.character(period_assigned)])
    }
  }

  if (!is.null(trtp_map)) {
    period_key <- as.character(period_assigned)
    src_cols <- unname(trtp_map[period_key])
    result[[trtp_var]] <- vapply(seq_len(nrow(result)), function(i) {
      col <- src_cols[i]
      if (is.na(col) || !col %in% names(result)) return(NA_character_)
      as.character(result[[col]][i])
    }, character(1))
    result[[trtp_var]][is_pre] <- NA_character_
  }

  if (!is.null(trta_map)) {
    period_key <- as.character(period_assigned)
    src_cols <- unname(trta_map[period_key])
    result[[trta_var]] <- vapply(seq_len(nrow(result)), function(i) {
      col <- src_cols[i]
      if (is.na(col) || !col %in% names(result)) return(NA_character_)
      as.character(result[[col]][i])
    }, character(1))
    result[[trta_var]][is_pre] <- NA_character_
  }

  result
}
