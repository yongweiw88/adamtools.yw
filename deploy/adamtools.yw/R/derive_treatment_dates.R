#' Derive treatment start/end dates from an Exposure domain
#'
#' Summarizes an SDTM `EX`-style exposure domain to subject-level overall
#' treatment start/end dates (`TRTSDT`/`TRTEDT`, first/latest date among
#' non-zero-dose records), and optionally per-period or per-epoch
#' `TRT<pp>SDT`/`TRT<pp>EDT` variables driven by a caller-supplied period or
#' epoch variable/map (never hardcoded period counts or epoch names).
#'
#' @param dataset An SDTM exposure domain data frame containing
#'   `subject_var`, `dose_var`, `start_dtc_var`, and `end_dtc_var`.
#' @param subject_var Name of the subject identifier column. Defaults to
#'   `"USUBJID"`.
#' @param dose_var Name of the numeric dose column used to exclude
#'   zero-dose (e.g. placebo-hold or interrupted) records from overall
#'   `TRTSDT`/`TRTEDT` derivation. Set to `NULL` to skip dose filtering
#'   (use all records). Defaults to `"EXDOSE"`.
#' @param start_dtc_var Name of the exposure start `--DTC` column. Defaults
#'   to `"EXSTDTC"`.
#' @param end_dtc_var Name of the exposure end `--DTC` column. Defaults to
#'   `"EXENDTC"`.
#' @param period_var Optional name of a period-number column in `dataset`
#'   (e.g. `"APERIOD"`) used to additionally derive `TRT<pp>SDT`/
#'   `TRT<pp>EDT` per distinct period value (zero-padded to 2 digits).
#'   Mutually exclusive with `epoch_var`. Defaults to `NULL`.
#' @param epoch_var Optional name of an epoch column in `dataset` (e.g.
#'   `"EPOCH"`) used together with `epoch_map` to derive named per-epoch
#'   `TRT<label>SDT`/`TRT<label>EDT` variables. Mutually exclusive with
#'   `period_var`. Defaults to `NULL`.
#' @param epoch_map Named character vector mapping raw `epoch_var` values to
#'   short suffixes used in the generated variable names (e.g. `c(
#'   "TREATMENT 1" = "01", "TREATMENT 2" = "02")`). Required when
#'   `epoch_var` is supplied.
#' @param date_imputation Passed to `admiral::convert_dtc_to_dt()` as
#'   `highest_imputation`; defaults to `"n"` (no imputation).
#'
#' @return A data frame with one row per distinct `subject_var` value,
#'   columns `TRTSDT`/`TRTEDT`, and (when `period_var` or `epoch_var` is
#'   supplied) additional per-period/per-epoch start/end date columns.
#' @export
derive_treatment_dates <- function(dataset,
                                   subject_var = "USUBJID",
                                   dose_var = "EXDOSE",
                                   start_dtc_var = "EXSTDTC",
                                   end_dtc_var = "EXENDTC",
                                   period_var = NULL,
                                   epoch_var = NULL,
                                   epoch_map = NULL,
                                   date_imputation = "n") {
  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }
  required <- c(subject_var, start_dtc_var, end_dtc_var)
  missing_vars <- setdiff(required, names(dataset))
  if (length(missing_vars) > 0L) {
    stop(
      "`dataset` is missing required column(s): ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.null(period_var) && !is.null(epoch_var)) {
    stop("Supply only one of `period_var` or `epoch_var`, not both.", call. = FALSE)
  }

  work <- dataset
  if (!is.null(dose_var) && dose_var %in% names(work)) {
    work <- work[!is.na(work[[dose_var]]) & work[[dose_var]] != 0, , drop = FALSE]
  }
  work$.ASTDT <- admiral::convert_dtc_to_dt(work[[start_dtc_var]], highest_imputation = date_imputation)
  end_dates <- admiral::convert_dtc_to_dt(work[[end_dtc_var]], highest_imputation = date_imputation)
  # Single-day EX records commonly omit EXENDTC; their start date is still
  # the record's latest treatment date.
  work$.AENDT <- pmax_date_na(work$.ASTDT, end_dates)

  summarize_dates <- function(data, prefix) {
    if (nrow(data) == 0L) {
      empty <- data.frame(character(0), as.Date(character(0)), as.Date(character(0)))
      names(empty) <- c(subject_var, paste0(prefix, "SDT"), paste0(prefix, "EDT"))
      return(empty)
    }
    data$.SUBJECT <- data[[subject_var]]
    agg_min <- stats::aggregate(.ASTDT ~ .SUBJECT, data = data, FUN = min, na.action = stats::na.omit)
    agg_max <- stats::aggregate(.AENDT ~ .SUBJECT, data = data, FUN = max, na.action = stats::na.omit)
    names(agg_min) <- c(subject_var, paste0(prefix, "SDT"))
    names(agg_max) <- c(subject_var, paste0(prefix, "EDT"))
    merge(agg_min, agg_max, by = subject_var, all = TRUE)
  }

  result <- summarize_dates(work, "TRT")

  if (!is.null(period_var)) {
    if (!period_var %in% names(dataset)) {
      stop("`period_var` is not a column in `dataset`.", call. = FALSE)
    }
    periods <- sort(unique(stats::na.omit(dataset[[period_var]])))
    for (p in periods) {
      suffix <- formatC(p, width = 2, flag = "0")
      period_data <- work[work[[period_var]] %in% p, , drop = FALSE]
      period_result <- summarize_dates(period_data, paste0("TRT", suffix))
      result <- merge(result, period_result, by = subject_var, all = TRUE)
    }
  }

  if (!is.null(epoch_var)) {
    if (!epoch_var %in% names(dataset)) {
      stop("`epoch_var` is not a column in `dataset`.", call. = FALSE)
    }
    if (is.null(epoch_map)) {
      stop("`epoch_map` is required when `epoch_var` is supplied.", call. = FALSE)
    }
    for (epoch_val in names(epoch_map)) {
      suffix <- epoch_map[[epoch_val]]
      epoch_data <- work[work[[epoch_var]] %in% epoch_val, , drop = FALSE]
      epoch_result <- summarize_dates(epoch_data, paste0("TRT", suffix))
      result <- merge(result, epoch_result, by = subject_var, all = TRUE)
    }
  }

  result
}
