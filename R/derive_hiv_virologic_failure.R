#' Derive configurable HIV virologic-failure flags
#'
#' Derives suspected and confirmed HIV virologic-failure flags from a
#' longitudinal BDS-like viral-load data frame. The function preserves all input
#' rows and adds one suspected and one confirmed flag for each caller-supplied
#' criterion, plus optional aggregate "any suspected" and "any confirmed" flags.
#'
#' The package intentionally does not provide default criteria. All clinical
#' constants and inclusion rules must be supplied by the caller in `criteria` and
#' the input data: viral-load threshold, eligible records (via `eligible_var` or
#' pre-filtered input), failure start day, suppression start day when it differs
#' from the failure start day, early-response end day, confirmation interval,
#' log10/nadir increment, and whether early inadequate-response confirmation
#' requires an external resistance flag. This keeps protocol-specific ADVF rules
#' in study programs while reusing the safe longitudinal mechanics.
#'
#' Supported criterion `type` values are:
#' \describe{
#'   \item{`"no_prior_confirmed_suppression"`}{Current value is at or above the
#'     threshold on/after `post_baseline_start_day` and there has not been prior
#'     confirmed suppression below the threshold.}
#'   \item{`"rebound_after_confirmed_suppression"`}{Current value is at or above
#'     the threshold on/after `post_baseline_start_day` after prior confirmed
#'     suppression below the threshold.}
#'   \item{`"above_nadir_increment"`}{Current log value is more than
#'     `log_nadir_increment` above the prior nadir log value among prior
#'     threshold-eligible values.}
#'   \item{`"early_inadequate_response"`}{Current record is within the caller's
#'     early-response window and shows less than the configured log drop from
#'     baseline. If `require_resistance` is `TRUE`, confirmed flags also require
#'     `resistance_var` to have a value in `resistance_values`.}
#' }
#'
#' A confirmed flag is set on the suspected record when a later eligible record
#' for the same subject, at least `min_confirmation_interval` days later, also
#' satisfies the relevant confirmation value rule. The function selects the first
#' such later eligible record deterministically after ordering by subject, date,
#' and explicit tie-break columns. If duplicate subject/date records exist
#' without `tie_break_vars`, or duplicate subject/date/tie-break combinations
#' remain, the function stops rather than silently choosing an arbitrary order.
#'
#' @param dataset BDS-like data frame with one row per subject/time point.
#' @param criteria Data frame with one row per criterion. Required columns:
#'   `criterion`, `type`, `threshold`, `post_baseline_start_day`, and
#'   `min_confirmation_interval`. Type-specific required columns are
#'   `log_nadir_increment` for `"above_nadir_increment"` and
#'   `"early_inadequate_response"`, `early_response_end_day` and
#'   `require_resistance` for `"early_inadequate_response"`. Optional columns
#'   `suppression_start_day` (for prior confirmed-suppression detection),
#'   `suspected_flag`, and `confirmed_flag` control output behavior; if flag
#'   names are omitted, names are derived from `criterion`.
#' @param subject_var Subject identifier column. Defaults to `"USUBJID"`.
#' @param aval_var Numeric analysis value column, usually HIV-1 RNA copies/mL.
#'   Defaults to `"AVAL"`.
#' @param date_var Analysis date column used for longitudinal ordering and
#'   confirmation intervals. Defaults to `"ADT"`.
#' @param day_var Numeric analysis day column. Defaults to `"ADY"`.
#' @param log_value_var Log-transformed value column, required by
#'   `"above_nadir_increment"`. Defaults to `NULL`.
#' @param log_change_var Log change-from-baseline column, required by
#'   `"early_inadequate_response"`. Defaults to `NULL`.
#' @param baseline_flag_var Baseline flag column used to exclude baseline rows
#'   from early inadequate-response flags. Required by
#'   `"early_inadequate_response"`. Defaults to `NULL`.
#' @param baseline_values Values in `baseline_flag_var` indicating a baseline
#'   record. Defaults to `"Y"`.
#' @param eligible_var Optional logical or flag column identifying rows eligible
#'   for virologic-failure derivation. If `NULL`, all input rows are treated as
#'   eligible, so callers should pass already filtered input when appropriate.
#' @param eligible_values Values in `eligible_var` treated as eligible when
#'   `eligible_var` is not logical. Defaults to `TRUE`.
#' @param resistance_var Optional external resistance flag/value column used only
#'   for criteria with `require_resistance = TRUE`.
#' @param resistance_values Values in `resistance_var` treated as resistance
#'   present. Defaults to `"Y"`.
#' @param tie_break_vars Character vector of explicit tie-break columns used
#'   after `date_var` within subject.
#' @param any_suspected_flag Optional aggregate suspected flag column name. Set
#'   to `NULL` to skip. Defaults to `"HIVVFSPFL"`.
#' @param any_confirmed_flag Optional aggregate confirmed flag column name. Set
#'   to `NULL` to skip. Defaults to `"HIVVFCFL"`.
#' @param true_value Value used for records meeting a criterion. Defaults to
#'   `"Y"`.
#' @param false_value Value used for records not meeting a criterion. Defaults
#'   to `NA_character_`.
#' @param overwrite Logical; allow output columns to replace existing columns.
#'   Defaults to `FALSE`.
#' @param keep_intermediates Logical; keep diagnostic columns for prior confirmed
#'   suppression and prior nadir per criterion. Defaults to `FALSE`.
#'
#' @return `dataset` with the configured virologic-failure flag variables added.
#' @examples
#' caller_threshold <- 123
#' caller_interval <- 5
#' criteria <- data.frame(
#'   criterion = "caller_defined_vf",
#'   type = "no_prior_confirmed_suppression",
#'   threshold = caller_threshold,
#'   post_baseline_start_day = 100,
#'   min_confirmation_interval = caller_interval,
#'   suspected_flag = "SVFFL",
#'   confirmed_flag = "CVFFL"
#' )
#' @export
derive_hiv_virologic_failure <- function(dataset,
                                         criteria,
                                         subject_var = "USUBJID",
                                         aval_var = "AVAL",
                                         date_var = "ADT",
                                         day_var = "ADY",
                                         log_value_var = NULL,
                                         log_change_var = NULL,
                                         baseline_flag_var = NULL,
                                         baseline_values = "Y",
                                         eligible_var = NULL,
                                         eligible_values = TRUE,
                                         resistance_var = NULL,
                                         resistance_values = "Y",
                                         tie_break_vars = NULL,
                                         any_suspected_flag = "HIVVFSPFL",
                                         any_confirmed_flag = "HIVVFCFL",
                                         true_value = "Y",
                                         false_value = NA_character_,
                                         overwrite = FALSE,
                                         keep_intermediates = FALSE) {
  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }
  if (!inherits(criteria, "data.frame")) {
    stop("`criteria` must be a data frame or tibble.", call. = FALSE)
  }

  required_dataset <- c(subject_var, aval_var, date_var, day_var, tie_break_vars)
  missing_dataset <- setdiff(required_dataset, names(dataset))
  if (length(missing_dataset) > 0L) {
    stop(
      "`dataset` is missing required column(s): ",
      paste(missing_dataset, collapse = ", "),
      call. = FALSE
    )
  }

  supported_types <- c(
    "no_prior_confirmed_suppression",
    "rebound_after_confirmed_suppression",
    "above_nadir_increment",
    "early_inadequate_response"
  )
  required_criteria <- c(
    "criterion",
    "type",
    "threshold",
    "post_baseline_start_day",
    "min_confirmation_interval"
  )
  missing_criteria <- setdiff(required_criteria, names(criteria))
  if (length(missing_criteria) > 0L) {
    stop(
      "`criteria` is missing required column(s): ",
      paste(missing_criteria, collapse = ", "),
      call. = FALSE
    )
  }
  if (nrow(criteria) == 0L) {
    stop("`criteria` must contain at least one row.", call. = FALSE)
  }
  if (any(is.na(criteria$criterion) | criteria$criterion == "")) {
    stop("`criteria$criterion` must be non-missing and non-empty.", call. = FALSE)
  }
  if (any(duplicated(criteria$criterion))) {
    stop("`criteria$criterion` values must be unique.", call. = FALSE)
  }
  bad_types <- setdiff(unique(criteria$type), supported_types)
  if (length(bad_types) > 0L) {
    stop(
      "Unsupported `criteria$type` value(s): ",
      paste(bad_types, collapse = ", "),
      call. = FALSE
    )
  }

  if (any(criteria$type == "above_nadir_increment") && is.null(log_value_var)) {
    stop(
      "`log_value_var` is required for criteria of type \"above_nadir_increment\".",
      call. = FALSE
    )
  }
  if (any(criteria$type == "early_inadequate_response")) {
    if (is.null(log_change_var)) {
      stop(
        "`log_change_var` is required for criteria of type \"early_inadequate_response\".",
        call. = FALSE
      )
    }
    if (is.null(baseline_flag_var)) {
      stop(
        "`baseline_flag_var` is required for criteria of type \"early_inadequate_response\".",
        call. = FALSE
      )
    }
    for (col in c("early_response_end_day", "log_nadir_increment", "require_resistance")) {
      if (!col %in% names(criteria)) {
        stop(
          "`criteria` is missing required column `", col,
          "` for type \"early_inadequate_response\".",
          call. = FALSE
        )
      }
    }
  }
  if (any(criteria$type == "above_nadir_increment") &&
      !"log_nadir_increment" %in% names(criteria)) {
    stop(
      "`criteria` is missing required column `log_nadir_increment` for type ",
      "\"above_nadir_increment\".",
      call. = FALSE
    )
  }

  optional_dataset <- c(log_value_var, log_change_var, baseline_flag_var, eligible_var, resistance_var)
  missing_optional <- setdiff(optional_dataset[!is.null(optional_dataset)], names(dataset))
  if (length(missing_optional) > 0L) {
    stop(
      "`dataset` is missing required column(s): ",
      paste(missing_optional, collapse = ", "),
      call. = FALSE
    )
  }

  require_resistance <- rep(FALSE, nrow(criteria))
  if ("require_resistance" %in% names(criteria)) {
    require_resistance <- .hiv_vf_get_logical(criteria, "require_resistance", default = FALSE)
  }
  if (any(require_resistance) && is.null(resistance_var)) {
    stop(
      "`resistance_var` is required when any criterion has `require_resistance = TRUE`.",
      call. = FALSE
    )
  }

  suspected_flags <- .hiv_vf_output_names(criteria, "suspected_flag", "SFL")
  confirmed_flags <- .hiv_vf_output_names(criteria, "confirmed_flag", "CFL")
  output_names <- c(suspected_flags, confirmed_flags, any_suspected_flag, any_confirmed_flag)
  output_names <- output_names[!is.na(output_names) & output_names != ""]
  if (any(duplicated(output_names))) {
    stop("Output flag column names must be unique.", call. = FALSE)
  }
  if (!overwrite) {
    existing_outputs <- intersect(output_names, names(dataset))
    if (length(existing_outputs) > 0L) {
      stop(
        "Output column(s) already exist in `dataset`: ",
        paste(existing_outputs, collapse = ", "),
        ". Set `overwrite = TRUE` to replace them.",
        call. = FALSE
      )
    }
  }

  if (!is.numeric(dataset[[aval_var]])) {
    stop("`aval_var` must identify a numeric column.", call. = FALSE)
  }
  if (!is.numeric(dataset[[day_var]])) {
    stop("`day_var` must identify a numeric column.", call. = FALSE)
  }
  if (!is.null(log_value_var) && !is.numeric(dataset[[log_value_var]])) {
    stop("`log_value_var` must identify a numeric column.", call. = FALSE)
  }
  if (!is.null(log_change_var) && !is.numeric(dataset[[log_change_var]])) {
    stop("`log_change_var` must identify a numeric column.", call. = FALSE)
  }

  eligible <- .hiv_vf_value_in(dataset, eligible_var, eligible_values)
  if (any(eligible & is.na(dataset[[date_var]]))) {
    stop("Eligible rows must have non-missing values in `date_var`.", call. = FALSE)
  }
  if (any(eligible & is.na(dataset[[subject_var]]))) {
    stop("Eligible rows must have non-missing values in `subject_var`.", call. = FALSE)
  }

  tie_key <- c(subject_var, date_var, tie_break_vars)
  if (is.null(tie_break_vars) && any(duplicated(dataset[c(subject_var, date_var)]))) {
    stop(
      "Duplicate subject/date records require explicit `tie_break_vars` for deterministic ordering.",
      call. = FALSE
    )
  }
  if (!is.null(tie_break_vars) && any(duplicated(dataset[tie_key]))) {
    stop(
      "`tie_break_vars` do not uniquely order records within subject/date.",
      call. = FALSE
    )
  }

  result <- dataset
  result$.hiv_vf_orig_row <- seq_len(nrow(result))
  result$.hiv_vf_eligible <- eligible

  order_key <- c(subject_var, date_var, tie_break_vars, ".hiv_vf_orig_row")
  ord <- do.call(base::order, c(result[order_key], list(na.last = TRUE)))
  result <- result[ord, , drop = FALSE]

  any_suspected <- rep(FALSE, nrow(result))
  any_confirmed <- rep(FALSE, nrow(result))

  for (i in seq_len(nrow(criteria))) {
    type <- criteria$type[i]
    threshold <- .hiv_vf_get_numeric(criteria, i, "threshold")
    start_day <- .hiv_vf_get_numeric(criteria, i, "post_baseline_start_day")
    min_interval <- .hiv_vf_get_numeric(criteria, i, "min_confirmation_interval")
    if (min_interval < 0) {
      stop("`min_confirmation_interval` must be non-negative.", call. = FALSE)
    }

    suspected <- rep(FALSE, nrow(result))
    confirmed <- rep(FALSE, nrow(result))
    prior_confirmed_suppression <- rep(FALSE, nrow(result))
    prior_nadir <- rep(NA_real_, nrow(result))

    split_rows <- split(seq_len(nrow(result)), result[[subject_var]], drop = TRUE)
    for (rows in split_rows) {
      group <- result[rows, , drop = FALSE]
      aval <- group[[aval_var]]
      day <- group[[day_var]]
      date_num <- .hiv_vf_date_to_day_number(group[[date_var]])
      group_eligible <- group$.hiv_vf_eligible

      current <- group_eligible & !is.na(aval) & !is.na(day) & day >= start_day
      suppression_start_day <- start_day
      if ("suppression_start_day" %in% names(criteria) &&
          !is.na(criteria[["suppression_start_day"]][i])) {
        suppression_start_day <- .hiv_vf_get_numeric(criteria, i, "suppression_start_day")
      }
      suppression_current <- group_eligible & !is.na(aval) & !is.na(day) & day >= suppression_start_day
      suppressed <- suppression_current & aval < threshold
      confirmed_suppressed <- .hiv_vf_confirmed_suppression(
        suppressed = suppressed,
        date_num = date_num,
        min_interval = min_interval
      )
      group_prior_suppression <- c(FALSE, head(cumsum(confirmed_suppressed) > 0L, -1L))

      next_confirm_idx <- .hiv_vf_next_confirmation_index(
        date_num = date_num,
        eligible = group_eligible,
        min_interval = min_interval
      )

      group_prior_nadir <- rep(NA_real_, length(rows))
      if (type == "above_nadir_increment") {
        increment <- .hiv_vf_get_numeric(criteria, i, "log_nadir_increment")
        log_value <- group[[log_value_var]]
        valid_nadir <- current & aval >= threshold & !is.na(log_value)
        for (j in seq_along(rows)) {
          prior_values <- log_value[seq_len(j - 1L)]
          prior_valid <- valid_nadir[seq_len(j - 1L)]
          if (any(prior_valid)) {
            group_prior_nadir[j] <- min(prior_values[prior_valid], na.rm = TRUE)
          }
        }
      }

      if (type == "no_prior_confirmed_suppression") {
        group_suspected <- current & aval >= threshold & !group_prior_suppression
        group_confirmed <- group_suspected &
          .hiv_vf_next_aval_ge(aval, next_confirm_idx, threshold)
      } else if (type == "rebound_after_confirmed_suppression") {
        group_suspected <- current & aval >= threshold & group_prior_suppression
        group_confirmed <- group_suspected &
          .hiv_vf_next_aval_ge(aval, next_confirm_idx, threshold)
      } else if (type == "above_nadir_increment") {
        increment <- .hiv_vf_get_numeric(criteria, i, "log_nadir_increment")
        log_value <- group[[log_value_var]]
        group_suspected <- current &
          !is.na(group_prior_nadir) &
          !is.na(log_value) &
          log_value > group_prior_nadir + increment
        group_confirmed <- group_suspected &
          .hiv_vf_next_log_above(log_value, next_confirm_idx, group_prior_nadir + increment)
      } else if (type == "early_inadequate_response") {
        increment <- .hiv_vf_get_numeric(criteria, i, "log_nadir_increment")
        early_end_day <- .hiv_vf_get_numeric(criteria, i, "early_response_end_day")
        log_change <- group[[log_change_var]]
        baseline <- .hiv_vf_value_in(group, baseline_flag_var, baseline_values)
        inadequate_response <- !is.na(log_change) & log_change > -increment
        group_suspected <- current &
          day <= early_end_day &
          !baseline &
          aval >= threshold &
          inadequate_response
        group_confirmed <- group_suspected &
          .hiv_vf_next_aval_ge(aval, next_confirm_idx, threshold) &
          .hiv_vf_next_log_change_inadequate(log_change, next_confirm_idx, increment)
        if (isTRUE(require_resistance[i])) {
          resistance_present <- .hiv_vf_value_in(group, resistance_var, resistance_values)
          group_confirmed <- group_confirmed & resistance_present
        }
      }

      suspected[rows] <- group_suspected
      confirmed[rows] <- group_confirmed
      prior_confirmed_suppression[rows] <- group_prior_suppression
      prior_nadir[rows] <- group_prior_nadir
    }

    result[[suspected_flags[i]]] <- ifelse(suspected, true_value, false_value)
    result[[confirmed_flags[i]]] <- ifelse(confirmed, true_value, false_value)
    any_suspected <- any_suspected | suspected
    any_confirmed <- any_confirmed | confirmed

    if (keep_intermediates) {
      prefix <- make.names(toupper(as.character(criteria$criterion[i])))
      result[[paste0(prefix, "_PRCSUPP")]] <- prior_confirmed_suppression
      result[[paste0(prefix, "_PRNADIR")]] <- prior_nadir
    }
  }

  if (!is.null(any_suspected_flag)) {
    result[[any_suspected_flag]] <- ifelse(any_suspected, true_value, false_value)
  }
  if (!is.null(any_confirmed_flag)) {
    result[[any_confirmed_flag]] <- ifelse(any_confirmed, true_value, false_value)
  }

  result <- result[order(result$.hiv_vf_orig_row), , drop = FALSE]
  result$.hiv_vf_orig_row <- NULL
  result$.hiv_vf_eligible <- NULL

  result
}

.hiv_vf_output_names <- function(criteria, column, suffix) {
  if (column %in% names(criteria)) {
    names <- as.character(criteria[[column]])
    missing <- is.na(names) | names == ""
  } else {
    names <- rep(NA_character_, nrow(criteria))
    missing <- rep(TRUE, nrow(criteria))
  }
  names[missing] <- paste0(make.names(toupper(as.character(criteria$criterion[missing]))), suffix)
  names
}

.hiv_vf_get_numeric <- function(criteria, row, column) {
  if (!column %in% names(criteria)) {
    stop("`criteria` is missing required column `", column, "`.", call. = FALSE)
  }
  value <- criteria[[column]][row]
  if (length(value) != 1L || is.na(value) || !is.numeric(value)) {
    stop("`criteria$", column, "` must contain non-missing numeric values.", call. = FALSE)
  }
  value
}

.hiv_vf_get_logical <- function(criteria, column, default = FALSE) {
  if (!column %in% names(criteria)) {
    return(rep(default, nrow(criteria)))
  }
  value <- criteria[[column]]
  if (is.logical(value)) {
    value[is.na(value)] <- default
    return(value)
  }
  if (is.character(value)) {
    upper <- toupper(value)
    out <- rep(default, length(value))
    out[upper %in% c("TRUE", "T", "Y", "YES", "1")] <- TRUE
    out[upper %in% c("FALSE", "F", "N", "NO", "0")] <- FALSE
    bad <- !(upper %in% c("TRUE", "T", "Y", "YES", "1", "FALSE", "F", "N", "NO", "0")) & !is.na(upper)
    if (any(bad)) {
      stop("`criteria$", column, "` must contain logical values.", call. = FALSE)
    }
    return(out)
  }
  stop("`criteria$", column, "` must contain logical values.", call. = FALSE)
}

.hiv_vf_value_in <- function(dataset, var, values) {
  if (is.null(var)) {
    return(rep(TRUE, nrow(dataset)))
  }
  x <- dataset[[var]]
  if (is.logical(x) && identical(values, TRUE)) {
    return(!is.na(x) & x)
  }
  !is.na(x) & as.character(x) %in% as.character(values)
}

.hiv_vf_date_to_day_number <- function(x) {
  if (inherits(x, "Date")) {
    return(as.numeric(x))
  }
  if (inherits(x, "POSIXt")) {
    return(as.numeric(as.Date(x)))
  }
  if (is.numeric(x)) {
    return(x)
  }
  stop("`date_var` must identify a Date, POSIXt, or numeric day column.", call. = FALSE)
}

.hiv_vf_confirmed_suppression <- function(suppressed, date_num, min_interval) {
  out <- rep(FALSE, length(suppressed))
  for (i in seq_along(suppressed)) {
    if (!suppressed[i]) next
    prior <- which(suppressed[seq_len(i - 1L)])
    if (length(prior) == 0L) next
    intervals <- date_num[i] - date_num[prior]
    out[i] <- any(!is.na(intervals) & intervals >= min_interval)
  }
  out
}

.hiv_vf_next_confirmation_index <- function(date_num, eligible, min_interval) {
  out <- rep(NA_integer_, length(eligible))
  eligible_rows <- which(eligible)
  for (i in seq_along(eligible)) {
    later <- eligible_rows[eligible_rows > i]
    if (length(later) == 0L) next
    intervals <- date_num[later] - date_num[i]
    valid <- later[!is.na(intervals) & intervals >= min_interval]
    if (length(valid) > 0L) {
      out[i] <- valid[1L]
    }
  }
  out
}

.hiv_vf_next_aval_ge <- function(aval, next_idx, threshold) {
  ok <- !is.na(next_idx)
  out <- rep(FALSE, length(aval))
  out[ok] <- !is.na(aval[next_idx[ok]]) & aval[next_idx[ok]] >= threshold
  out
}

.hiv_vf_next_log_above <- function(log_value, next_idx, cutoff) {
  ok <- !is.na(next_idx)
  out <- rep(FALSE, length(log_value))
  out[ok] <- !is.na(log_value[next_idx[ok]]) &
    !is.na(cutoff[ok]) &
    log_value[next_idx[ok]] > cutoff[ok]
  out
}

.hiv_vf_next_log_change_inadequate <- function(log_change, next_idx, increment) {
  ok <- !is.na(next_idx)
  out <- rep(FALSE, length(log_change))
  out[ok] <- !is.na(log_change[next_idx[ok]]) &
    log_change[next_idx[ok]] > -increment
  out
}
