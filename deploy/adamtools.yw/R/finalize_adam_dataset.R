#' Finalize an ADaM dataset against a metacore specification
#'
#' Orchestrates the repeated metadata-finalization pipeline seen across study
#' programs (often duplicated locally as a `fin_prep()`-style helper): drop
#' variables not in the spec, check variables against the spec, order
#' columns per the spec, sort by key variables, and apply `xportr`
#' type/length/label/format/dataset-label attributes. This function composes
#' existing `metatools`/`metacore`/`xportr` building blocks; it does not
#' reimplement any of their internal logic.
#'
#' @param dataset A data frame or tibble; the ADaM dataset to finalize.
#' @param domain Dataset domain, e.g. `"ADAE"`. Used to select the dataset
#'   spec from `metacore` and passed through to `xportr` calls.
#' @param metacore A `metacore` spec object (e.g. from
#'   `metacore::spec_to_metacore()`), already filtered or not to `domain`;
#'   if not yet filtered, it is filtered internally via
#'   `metacore::select_dataset()`.
#' @param drop_unspec Logical; drop variables in `dataset` that are not
#'   present in the `metacore` spec for `domain`, via
#'   `metatools::drop_unspec_vars()`. Defaults to `TRUE`.
#' @param check Logical; run `metatools::check_variables()` to confirm
#'   `dataset` variables match the spec. Defaults to `TRUE`.
#' @param order_cols Logical; reorder columns to match the spec via
#'   `metatools::order_cols()`. Defaults to `TRUE`.
#' @param sort_keys Character vector of key variables to sort `dataset` by
#'   (via `metatools::sort_by_key()`), typically the spec's key sequence.
#'   If `NULL` (default), sorting is skipped.
#' @param apply_xportr_attrs Logical; apply `xportr` type/length/label/
#'   format/dataset-label metadata via `xportr::xportr_type()`,
#'   `xportr::xportr_length()`, `xportr::xportr_label()`,
#'   `xportr::xportr_format()`, and `xportr::xportr_df_label()`. Defaults to
#'   `TRUE`.
#'
#' @return The finalized data frame or tibble.
#' @export
finalize_adam_dataset <- function(dataset,
                                  domain,
                                  metacore,
                                  drop_unspec = TRUE,
                                  check = TRUE,
                                  order_cols = TRUE,
                                  sort_keys = NULL,
                                  apply_xportr_attrs = TRUE) {
  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }
  if (missing(domain) || is.null(domain) || !nzchar(domain)) {
    stop("`domain` is required.", call. = FALSE)
  }
  if (missing(metacore) || is.null(metacore)) {
    stop("`metacore` is required.", call. = FALSE)
  }

  ds_spec <- tryCatch(
    metacore::select_dataset(metacore, domain),
    error = function(e) metacore
  )

  result <- dataset

  if (drop_unspec) {
    result <- metatools::drop_unspec_vars(result, ds_spec)
  }
  if (check) {
    result <- metatools::check_variables(result, ds_spec)
  }
  if (order_cols) {
    result <- metatools::order_cols(result, ds_spec)
  }
  if (!is.null(sort_keys)) {
    sort_keys <- intersect(sort_keys, names(result))
    if (length(sort_keys) > 0L) {
      result <- metatools::sort_by_key(result, sort_keys)
    }
  }
  if (apply_xportr_attrs) {
    result <- xportr::xportr_type(result, ds_spec, domain = domain)
    result <- xportr::xportr_length(result, ds_spec, domain = domain)
    result <- xportr::xportr_label(result, ds_spec, domain = domain)
    result <- xportr::xportr_format(result, ds_spec, domain = domain)
    result <- xportr::xportr_df_label(result, ds_spec, domain = domain)
  }

  result
}
