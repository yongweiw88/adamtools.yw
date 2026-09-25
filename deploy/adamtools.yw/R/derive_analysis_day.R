#' Derive analysis day relative to a reference date
#'
#' Creates a study day variable such as `ADY` following the ADaM day-numbering
#' convention (there is no "Day 0": the reference date itself is day 1, and
#' dates on/after the reference date are `diff + 1`; dates before the
#' reference date keep their negative day count). This is a thin, simplified
#' wrapper around `admiral::derive_vars_dy()` so callers can pass a single
#' date/anchor pair instead of building an `exprs()` list, while still
#' reusing admiral's conformant day-numbering logic. It is designed to be
#' generic across ADaM datasets, including studies with multiple phases or
#' periods where the anchor date may vary by row or dataset (e.g. a
#' period-specific `TRT0#SDT`).
#'
#' @param dataset A data frame or tibble.
#' @param date_var Name of the date/datetime variable to calculate the
#'   analysis day from (unquoted).
#' @param anchor_var Name of the reference date/datetime variable, such as
#'   `TRTSDT` (unquoted).
#' @param new_var Name of the new analysis-day variable. Defaults to `"ADY"`.
#'
#' @return The input dataset with the new analysis-day variable added.
#' @export
derive_analysis_day <- function(dataset,
                               date_var,
                               anchor_var,
                               new_var = "ADY") {
  date_sym <- rlang::ensym(date_var)
  anchor_sym <- rlang::ensym(anchor_var)

  date_name <- rlang::as_name(date_sym)
  anchor_name <- rlang::as_name(anchor_sym)
  new_var_name <- new_var

  if (!date_name %in% names(dataset)) {
    stop("`date_var` is not a column in `dataset`.", call. = FALSE)
  }

  if (!anchor_name %in% names(dataset)) {
    stop("`anchor_var` is not a column in `dataset`.", call. = FALSE)
  }

  source_var <- rlang::inject(rlang::exprs(!!new_var_name := !!date_sym))

  admiral::derive_vars_dy(
    dataset = dataset,
    reference_date = !!anchor_sym,
    source_vars = source_var
  )
}
