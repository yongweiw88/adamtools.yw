#' Derive an ADaM sequence number variable
#'
#' Adds a sequence number variable (e.g. `ASEQ`) reflecting the current row
#' order of `dataset`, optionally restarting the count within groups. This
#' formalizes the very common `mutate(ASEQ = row_number())` pattern found
#' across ADaM production and QC programs, avoiding inconsistent numeric
#' typing (`row_number()` vs. `as.numeric(row_number())`) between programs.
#' This function does **not** sort `dataset`; callers must arrange rows into
#' the desired order (e.g. with `dplyr::arrange()`) before calling.
#'
#' @param dataset A data frame or tibble, already sorted into the desired
#'   output order.
#' @param var Name of the new sequence variable to create (unquoted).
#'   Defaults to `ASEQ`.
#' @param by Optional character vector of grouping variables; when supplied,
#'   the sequence restarts at 1 within each group (e.g. `c("USUBJID")` for a
#'   per-subject sequence). If `NULL` (default), the sequence runs over the
#'   whole dataset.
#' @param as_numeric Logical; store the sequence as `numeric` (`TRUE`,
#'   default, matching common ADaM `Num` variable typing) or `integer`
#'   (`FALSE`).
#'
#' @return `dataset` with the sequence variable added.
#' @export
derive_aseq <- function(dataset, var = ASEQ, by = NULL, as_numeric = TRUE) {
  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }

  var_sym <- rlang::ensym(var)
  var_name <- rlang::as_name(var_sym)

  if (!is.null(by)) {
    missing_by <- setdiff(by, names(dataset))
    if (length(missing_by) > 0L) {
      stop(
        "The following `by` variables are not in `dataset`: ",
        paste(missing_by, collapse = ", "),
        call. = FALSE
      )
    }
  }

  result <- dataset
  if (is.null(by)) {
    seq_vals <- seq_len(nrow(result))
  } else {
    grouped <- dplyr::group_by(dplyr::mutate(result, .row_order_ = dplyr::row_number()), dplyr::across(dplyr::all_of(by)))
    seq_vals <- dplyr::mutate(grouped, .seq_ = dplyr::row_number())
    seq_vals <- dplyr::ungroup(seq_vals)
    seq_vals <- seq_vals[order(seq_vals$.row_order_), ]$.seq_
  }

  if (as_numeric) {
    seq_vals <- as.numeric(seq_vals)
  } else {
    seq_vals <- as.integer(seq_vals)
  }

  result[[var_name]] <- seq_vals
  result
}
