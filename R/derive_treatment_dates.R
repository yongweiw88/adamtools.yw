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
#'   Mutually exclusive with `epoch_var` and `switch_var`. Defaults to
#'   `NULL`.
#' @param epoch_var Optional name of an epoch column in `dataset` (e.g.
#'   `"EPOCH"`) used together with `epoch_map` to derive named per-epoch
#'   `TRT<label>SDT`/`TRT<label>EDT` variables. Mutually exclusive with
#'   `period_var` and `switch_var`. Defaults to `NULL`.
#' @param epoch_map Named character vector mapping raw `epoch_var` values to
#'   short suffixes used in the generated variable names (e.g. `c(
#'   "TREATMENT 1" = "01", "TREATMENT 2" = "02")`). Required when
#'   `epoch_var` is supplied.
#' @param switch_var Optional name of a numeric column in `dataset` (e.g.
#'   `"VISITNUM"`) used, together with `switch_cutoff`, to detect a
#'   two-period boundary from a **dosing-cadence change** rather than an
#'   `EPOCH`/period value. Some long-acting-injectable designs (e.g. a
#'   switch from monthly to bimonthly dosing) do not reliably tag the new
#'   cadence with a distinct `EPOCH`, so the boundary must instead be
#'   detected from the exposure records themselves: the first dose record
#'   with `switch_var >= switch_cutoff` marks the start of period 2.
#'   Derives `TR01SDT` (= overall `TRTSDT`), `TR01EDT` (switch date if any
#'   record meets the cutoff, else overall `TRTEDT`), `TR02SDT`/`TR02EDT`
#'   (switch date / overall `TRTEDT`, both `NA` if no record meets the
#'   cutoff). Mutually exclusive with `period_var` and `epoch_var`.
#'   Defaults to `NULL`.
#' @param switch_cutoff Numeric threshold for `switch_var`; required when
#'   `switch_var` is supplied. See `switch_var`.
#' @param date_imputation Passed to `admiral::convert_dtc_to_dt()` as
#'   `highest_imputation`; defaults to `"n"` (no imputation).
#'
#' @return A data frame with one row per distinct `subject_var` value,
#'   columns `TRTSDT`/`TRTEDT`, and (when `period_var`, `epoch_var`, or
#'   `switch_var` is supplied) additional per-period/per-epoch/per-switch
#'   start/end date columns.
#' @export
derive_treatment_dates <- function(dataset,
                                   subject_var = "USUBJID",
                                   dose_var = "EXDOSE",
                                   start_dtc_var = "EXSTDTC",
                                   end_dtc_var = "EXENDTC",
                                   period_var = NULL,
                                   epoch_var = NULL,
                                   epoch_map = NULL,
                                   switch_var = NULL,
                                   switch_cutoff = NULL,
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
  supplied <- c(!is.null(period_var), !is.null(epoch_var), !is.null(switch_var))
  if (sum(supplied) > 1L) {
    stop("Supply only one of `period_var`, `epoch_var`, or `switch_var`, not more than one.", call. = FALSE)
  }
  if (!is.null(switch_var) && is.null(switch_cutoff)) {
    stop("`switch_cutoff` is required when `switch_var` is supplied.", call. = FALSE)
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

  if (!is.null(switch_var)) {
    if (!switch_var %in% names(dataset)) {
      stop("`switch_var` is not a column in `dataset`.", call. = FALSE)
    }
    work$.SUBJECT <- work[[subject_var]]
    work$.SWITCH_MET <- !is.na(work[[switch_var]]) & work[[switch_var]] >= switch_cutoff
    switch_dt <- stats::aggregate(
      .ASTDT ~ .SUBJECT,
      data = work[work$.SWITCH_MET, , drop = FALSE],
      FUN = min,
      na.action = stats::na.omit
    )
    names(switch_dt) <- c(subject_var, ".SWITCHDT")
    result <- merge(result, switch_dt, by = subject_var, all.x = TRUE)
    has_switch <- !is.na(result$.SWITCHDT)
    result$TR01SDT <- result$TRTSDT
    result$TR01EDT <- ifelse(has_switch, result$.SWITCHDT, result$TRTEDT)
    result$TR02SDT <- ifelse(has_switch, result$.SWITCHDT, NA)
    result$TR02EDT <- ifelse(has_switch, result$TRTEDT, NA)
    for (v in c("TR01EDT", "TR02SDT", "TR02EDT")) result[[v]] <- as.Date(result[[v]], origin = "1970-01-01")
    result$.SWITCHDT <- NULL
  }

  result
}
