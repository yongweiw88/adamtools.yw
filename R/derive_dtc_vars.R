#' Derive dates, imputation flags, datetimes, and times from SDTM DTC values
#'
#' These helpers standardize the recurring ADSL/ADaM pattern of converting
#' CDISC ISO 8601 `--DTC` character values to analysis dates and optional
#' imputation flags, while leaving all study-specific imputation choices to
#' the caller. Date/datetime conversion is delegated to `admiral` so partial
#' date handling remains aligned with the ADaM stack; the package helper adds
#' multi-variable mapping, blank handling, and a lightweight ADaM-style date
#' imputation flag for year-only (`"M"`) and year-month (`"D"`) source values.
#'
#' @param dtc Character vector of SDTM `--DTC` values.
#' @param missing_flag Value used when no date component was imputed. Defaults
#'   to `""`, matching common ADaM character flag conventions.
#'
#' @return For `dtc_date_imputation_flag()`, a character vector with `"M"`
#'   for year-only dates, `"D"` for year-month dates, and `missing_flag`
#'   otherwise.
#' @export
dtc_date_imputation_flag <- function(dtc, missing_flag = "") {
  x <- .adamtools_clean_dtc(dtc)
  precision <- .adamtools_dtc_date_precision(x)

  out <- rep(missing_flag, length(x))
  out[precision == "year"] <- "M"
  out[precision == "month"] <- "D"
  out
}

#' @rdname dtc_date_imputation_flag
#'
#' @param dataset A data frame or tibble.
#' @param mappings Named character vector mapping source `--DTC` column names
#'   to output `Date` column names, e.g. `c(BRTHDTC = "BRTHDT")`.
#' @param flag_mappings Optional named character vector mapping source `--DTC`
#'   column names to output date-imputation-flag column names, e.g.
#'   `c(BRTHDTC = "BRTHDTF")`. If `TRUE`, flags are created for every entry in
#'   `mappings` by appending `"F"` to the output date variable name. If `NULL`
#'   (default), no flags are created.
#' @param highest_imputation Passed to `admiral::convert_dtc_to_dt()` as
#'   `highest_imputation`. Defaults to `"n"` (no date imputation).
#' @param date_imputation Passed to `admiral::convert_dtc_to_dt()` as
#'   `date_imputation`. Typical values are `"first"`, `"mid"`, or `"last"`.
#' @param overwrite Logical; allow output columns to replace existing columns
#'   in `dataset`. Defaults to `FALSE`.
#'
#' @return `derive_dtc_date_vars()` returns `dataset` with the requested date
#'   and optional flag columns added.
#' @export
derive_dtc_date_vars <- function(dataset,
                                 mappings,
                                 flag_mappings = NULL,
                                 highest_imputation = "n",
                                 date_imputation = "first",
                                 missing_flag = "",
                                 overwrite = FALSE) {
  .adamtools_check_data_frame(dataset)
  .adamtools_check_named_character(mappings, "mappings")
  .adamtools_check_columns(dataset, names(mappings))

  if (isTRUE(flag_mappings)) {
    flag_mappings <- stats::setNames(paste0(unname(mappings), "F"), names(mappings))
  } else if (isFALSE(flag_mappings)) {
    flag_mappings <- NULL
  } else if (!is.null(flag_mappings)) {
    .adamtools_check_named_character(flag_mappings, "flag_mappings")
    missing_flag_sources <- setdiff(names(flag_mappings), names(mappings))
    if (length(missing_flag_sources) > 0L) {
      stop(
        "`flag_mappings` source column(s) are not present in `mappings`: ",
        paste(missing_flag_sources, collapse = ", "),
        call. = FALSE
      )
    }
  }

  output_cols <- c(unname(mappings), unname(flag_mappings))
  .adamtools_check_output_columns(dataset, output_cols, overwrite = overwrite)

  result <- dataset
  for (source_var in names(mappings)) {
    dtc <- .adamtools_clean_dtc(result[[source_var]])
    result[[mappings[[source_var]]]] <- admiral::convert_dtc_to_dt(
      dtc,
      highest_imputation = highest_imputation,
      date_imputation = date_imputation
    )
    if (!is.null(flag_mappings) && source_var %in% names(flag_mappings)) {
      result[[flag_mappings[[source_var]]]] <- dtc_date_imputation_flag(
        dtc,
        missing_flag = missing_flag
      )
    }
  }

  result
}

#' @rdname dtc_date_imputation_flag
#'
#' @param time_mappings Optional named character vector mapping source `--DTC`
#'   column names to output time column names. If `TRUE`, a time column is
#'   created for every datetime output by replacing a trailing `"DTM"` suffix
#'   with `"TM"` (e.g. `EXSTDTM` -> `EXSTTM`). If `NULL` (default), no time
#'   variables are created.
#' @param time_imputation Passed to `admiral::convert_dtc_to_dtm()` as
#'   `time_imputation`. Defaults to `"first"`.
#' @param timezone Time zone used when extracting clock time from POSIXct
#'   datetimes. Defaults to `"UTC"`.
#'
#' @return `derive_dtc_datetime_vars()` returns `dataset` with the requested
#'   POSIXct datetime columns and optional `hms` time columns added.
#' @export
derive_dtc_datetime_vars <- function(dataset,
                                     mappings,
                                     time_mappings = NULL,
                                     highest_imputation = "s",
                                     date_imputation = "first",
                                     time_imputation = "first",
                                     timezone = "UTC",
                                     overwrite = FALSE) {
  .adamtools_check_data_frame(dataset)
  .adamtools_check_named_character(mappings, "mappings")
  .adamtools_check_columns(dataset, names(mappings))

  if (isTRUE(time_mappings)) {
    time_names <- sub("DTM$", "TM", unname(mappings))
    unchanged <- identical(time_names, unname(mappings))
    if (unchanged) {
      stop(
        "Automatic `time_mappings = TRUE` requires datetime output names ending in `DTM`.",
        call. = FALSE
      )
    }
    time_mappings <- stats::setNames(time_names, names(mappings))
  } else if (isFALSE(time_mappings)) {
    time_mappings <- NULL
  } else if (!is.null(time_mappings)) {
    .adamtools_check_named_character(time_mappings, "time_mappings")
    missing_time_sources <- setdiff(names(time_mappings), names(mappings))
    if (length(missing_time_sources) > 0L) {
      stop(
        "`time_mappings` source column(s) are not present in `mappings`: ",
        paste(missing_time_sources, collapse = ", "),
        call. = FALSE
      )
    }
  }

  output_cols <- c(unname(mappings), unname(time_mappings))
  .adamtools_check_output_columns(dataset, output_cols, overwrite = overwrite)

  result <- dataset
  for (source_var in names(mappings)) {
    dtm_var <- mappings[[source_var]]
    dtc <- .adamtools_clean_dtc(result[[source_var]])
    result[[dtm_var]] <- admiral::convert_dtc_to_dtm(
      dtc,
      highest_imputation = highest_imputation,
      date_imputation = date_imputation,
      time_imputation = time_imputation
    )
    if (!is.null(time_mappings) && source_var %in% names(time_mappings)) {
      result[[time_mappings[[source_var]]]] <- .adamtools_dtm_to_hms(
        result[[dtm_var]],
        timezone = timezone
      )
    }
  }

  result
}

.adamtools_clean_dtc <- function(dtc) {
  x <- as.character(dtc)
  x[trimws(x) == ""] <- NA_character_
  trimws(x)
}

.adamtools_dtc_date_precision <- function(dtc) {
  x <- .adamtools_clean_dtc(dtc)
  out <- rep("invalid", length(x))
  out[is.na(x)] <- NA_character_

  is_year <- !is.na(x) & grepl("^[0-9]{4}$", x)
  is_month <- !is.na(x) & grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", x)
  is_day_syntax <- !is.na(x) & grepl("^[0-9]{4}-(0[1-9]|1[0-2])-[0-9]{2}($|T)", x)
  day_part <- substr(x[is_day_syntax], 1L, 10L)
  valid_day <- !is.na(suppressWarnings(as.Date(day_part, format = "%Y-%m-%d")))

  out[is_year] <- "year"
  out[is_month] <- "month"
  day_idx <- which(is_day_syntax)
  out[day_idx[valid_day]] <- "day"
  out
}

.adamtools_dtm_to_hms <- function(dtm, timezone = "UTC") {
  out <- rep(NA_real_, length(dtm))
  ok <- !is.na(dtm)
  if (any(ok)) {
    posix <- as.POSIXlt(dtm[ok], tz = timezone)
    out[ok] <- posix$hour * 3600 + posix$min * 60 + floor(posix$sec)
  }
  hms::as_hms(out)
}

.adamtools_check_named_character <- function(x, arg) {
  if (!is.character(x) || length(x) == 0L ||
      is.null(names(x)) || any(is.na(names(x))) ||
      any(names(x) == "") || any(is.na(x)) || any(x == "")) {
    stop("`", arg, "` must be a non-empty named character vector.", call. = FALSE)
  }
  invisible(TRUE)
}
