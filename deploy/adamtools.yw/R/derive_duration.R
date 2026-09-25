#' Derive duration between two dates in a requested unit
#'
#' Derives a numeric duration (`ADURN`) and its unit (`ADURU`) between a
#' start and end date, defaulting to an in-progress cutoff date when the end
#' date is missing. Follows the ADaM convention that a duration in days is
#' inclusive of both start and end dates (`end - start + 1`); week/month/year
#' durations are computed from the day count using fixed average-length
#' divisors (7, 30.4375, 365.25 respectively).
#'
#' @param dataset A data frame or tibble containing `start_var` and
#'   `end_var`.
#' @param start_var Name of the start-date column (unquoted).
#' @param end_var Name of the end-date column (unquoted). Rows with a
#'   missing end date use `cutoff_date` instead, when supplied.
#' @param unit Duration unit: one of `"DAYS"`, `"WEEKS"`, `"MONTHS"`,
#'   `"YEARS"`. Defaults to `"DAYS"`.
#' @param cutoff_date Optional single `Date` (or a column name, unquoted,
#'   evaluated within `dataset`) used in place of a missing `end_var`, e.g.
#'   a data-cutoff date for ongoing subjects. Defaults to `NULL` (rows with
#'   missing `end_var` get `NA` duration).
#' @param round_down Logical; if `TRUE`, floor the WEEKS/MONTHS/YEARS result
#'   to a whole number. Ignored for `unit = "DAYS"`. Defaults to `FALSE`.
#' @param durn_var Name of the new numeric duration variable. Defaults to
#'   `"ADURN"`.
#' @param duru_var Name of the new duration-unit variable. Defaults to
#'   `"ADURU"`.
#'
#' @return `dataset` with `durn_var` and `duru_var` added.
#' @export
derive_duration <- function(dataset,
                            start_var,
                            end_var,
                            unit = "DAYS",
                            cutoff_date = NULL,
                            round_down = FALSE,
                            durn_var = "ADURN",
                            duru_var = "ADURU") {
  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }
  unit <- match.arg(unit, c("DAYS", "WEEKS", "MONTHS", "YEARS"))

  start_sym <- rlang::ensym(start_var)
  end_sym <- rlang::ensym(end_var)
  start_name <- rlang::as_name(start_sym)
  end_name <- rlang::as_name(end_sym)

  if (!start_name %in% names(dataset)) {
    stop("`start_var` is not a column in `dataset`.", call. = FALSE)
  }
  if (!end_name %in% names(dataset)) {
    stop("`end_var` is not a column in `dataset`.", call. = FALSE)
  }

  start_vals <- dataset[[start_name]]
  end_vals <- dataset[[end_name]]

  cutoff_arg <- rlang::enquo(cutoff_date)
  if (!rlang::quo_is_null(cutoff_arg)) {
    cutoff_vals <- rlang::eval_tidy(cutoff_arg, data = dataset)
    if (length(cutoff_vals) == 1L) cutoff_vals <- rep(cutoff_vals, nrow(dataset))
    end_vals <- ifelse(is.na(end_vals), cutoff_vals, end_vals)
    class(end_vals) <- class(dataset[[end_name]])
  }

  days <- as.numeric(end_vals - start_vals) + 1

  durn <- switch(unit,
    DAYS = days,
    WEEKS = days / 7,
    MONTHS = days / 30.4375,
    YEARS = days / 365.25
  )

  if (round_down && unit != "DAYS") {
    durn <- base::floor(durn)
  }

  dataset[[durn_var]] <- durn
  dataset[[duru_var]] <- ifelse(is.na(durn), NA_character_, unit)
  dataset
}
