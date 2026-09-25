#' Rowwise minimum/maximum across date vectors with safe all-missing handling
#'
#' Thin wrappers around `pmin()`/`pmax()` for date vectors that make the
#' "all values missing" behavior explicit and preserve the `Date` class,
#' rather than relying on ad hoc `pmin(..., na.rm = TRUE)`/
#' `pmax(..., na.rm = TRUE)` calls repeated across ADaM and TTE programs
#' (which return `-Inf`/`Inf`, not `NA`, when every input is `NA`).
#'
#' @param ... One or more `Date` vectors of the same length.
#' @param all_na Value to return for rows where every input is `NA`.
#' Defaults to `as.Date(NA)`.
#'
#' @return A `Date` vector of the rowwise minimum/maximum, with `all_na` used
#'   for rows where every input value is `NA`.
#' @export
pmin_date_na <- function(..., all_na = as.Date(NA)) {
  p_date_extreme(..., direction = "min", all_na = all_na)
}

#' @rdname pmin_date_na
#' @export
pmax_date_na <- function(..., all_na = as.Date(NA)) {
  p_date_extreme(..., direction = "max", all_na = all_na)
}

#' @rdname pmin_date_na
#' @param direction Either `"min"` or `"max"`.
#' @export
p_date_extreme <- function(..., direction = c("min", "max"), all_na = as.Date(NA)) {
  direction <- match.arg(direction)
  dots <- list(...)
  if (length(dots) == 0L) {
    stop("At least one date vector must be supplied to `...`.", call. = FALSE)
  }

  lengths <- vapply(dots, length, integer(1))
  if (length(unique(lengths)) > 1L) {
    stop("All vectors passed to `...` must be the same length.", call. = FALSE)
  }

  mat <- do.call(cbind, lapply(dots, function(x) as.numeric(as.Date(x))))
  all_missing <- apply(mat, 1, function(x) all(is.na(x)))

  fn <- if (identical(direction, "min")) pmin else pmax
  result_num <- suppressWarnings(
    do.call(fn, c(lapply(dots, function(x) as.numeric(as.Date(x))), list(na.rm = TRUE)))
  )
  result_num[!is.finite(result_num)] <- NA_real_

  result <- as.Date(result_num, origin = "1970-01-01")
  result[all_missing] <- all_na

  result
}
