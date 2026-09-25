#' Strip variable attributes from a data frame
#'
#' Removes attributes such as `label` and `format.sas` from selected (or all)
#' variables in a data frame, useful before comparing two datasets whose
#' values match but whose SAS/haven/xportr-style metadata attributes differ.
#' This formalizes the repeated inline attribute-stripping seen before
#' `diffdf::diffdf()` calls in QC programs; see [compare_adam_dataset()]
#' for a wrapper that applies this automatically.
#'
#' @param dataset A data frame or tibble.
#' @param vars Character vector of variable names to strip attributes from.
#'   If `NULL` (default), all variables in `dataset` are stripped.
#' @param attrs Character vector of attribute names to remove. Defaults to
#'   `c("label", "format.sas")`.
#'
#' @return `dataset` with the specified attributes removed from the
#'   specified (or all) variables. Column classes are preserved.
#' @export
strip_var_attrs <- function(dataset,
                            vars = NULL,
                            attrs = c("label", "format.sas")) {
  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }

  if (is.null(vars)) {
    vars <- names(dataset)
  } else {
    missing_vars <- setdiff(vars, names(dataset))
    if (length(missing_vars) > 0L) {
      stop(
        "The following `vars` are not in `dataset`: ",
        paste(missing_vars, collapse = ", "),
        call. = FALSE
      )
    }
  }

  for (v in vars) {
    for (a in attrs) {
      attr(dataset[[v]], a) <- NULL
    }
  }

  dataset
}
