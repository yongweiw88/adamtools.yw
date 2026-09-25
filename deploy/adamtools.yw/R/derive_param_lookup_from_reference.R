#' Build a TESTCD-to-PARAMCD/PARAM lookup from a reference ADaM dataset
#'
#' Derives a parameter code/label lookup table from an existing (reference)
#' ADaM BDS dataset, by extracting the distinct combination of test code and
#' parameter code/label/number columns. This avoids hardcoding parameter
#' mappings in derivation code and lets a single reference dataset (e.g. a
#' previous study's ADaM extract, or a metadata dataset) drive the mapping
#' for [derive_bds_from_findings()]'s `param_map` argument.
#'
#' @param ref A reference data frame containing at least `testcd_var` and
#'   `paramcd_var`.
#' @param testcd_var Name of the source test-code column in `ref` (e.g.
#'   `"LBTESTCD"`). Defaults to `"TESTCD"`.
#' @param paramcd_var Name of the ADaM parameter-code column in `ref`.
#'   Defaults to `"PARAMCD"`.
#' @param param_var Name of the ADaM parameter-label column in `ref`, if
#'   present. Defaults to `"PARAM"`.
#' @param paramn_var Name of the ADaM parameter-number column in `ref`, if
#'   present. Defaults to `"PARAMN"`.
#' @param keep_paramcd Optional character vector of `PARAMCD` values to
#'   retain. If `NULL` (default), all distinct parameters found in `ref`
#'   are kept.
#' @param extra_keys Optional character vector of additional column names in
#'   `ref` to include in the returned lookup and treat as part of the join
#'   key (e.g. `c("LBSPEC", "LBMETHOD")` for lab tests that require a
#'   compound key beyond `TESTCD` alone).
#'
#' @return A data frame with columns `TESTCD`, `PARAMCD`, `PARAM` (if
#'   available), `PARAMN` (if available), and any `extra_keys`, containing
#'   one row per distinct combination found in `ref`.
#' @export
derive_param_lookup_from_reference <- function(ref,
                                               testcd_var = "TESTCD",
                                               paramcd_var = "PARAMCD",
                                               param_var = "PARAM",
                                               paramn_var = "PARAMN",
                                               keep_paramcd = NULL,
                                               extra_keys = NULL) {
  if (!inherits(ref, "data.frame")) {
    stop("`ref` must be a data frame or tibble.", call. = FALSE)
  }
  required <- c(testcd_var, paramcd_var)
  missing_vars <- setdiff(required, names(ref))
  if (length(missing_vars) > 0L) {
    stop(
      "`ref` is missing required column(s): ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.null(extra_keys)) {
    missing_extra <- setdiff(extra_keys, names(ref))
    if (length(missing_extra) > 0L) {
      stop(
        "`extra_keys` refers to column(s) not found in `ref`: ",
        paste(missing_extra, collapse = ", "),
        call. = FALSE
      )
    }
  }

  cols <- c(testcd_var, paramcd_var)
  if (param_var %in% names(ref)) cols <- c(cols, param_var)
  if (paramn_var %in% names(ref)) cols <- c(cols, paramn_var)
  if (!is.null(extra_keys)) cols <- c(cols, extra_keys)

  lookup <- unique(ref[cols])

  rename_map <- stats::setNames(c(testcd_var, paramcd_var), c("TESTCD", "PARAMCD"))
  if (param_var %in% names(lookup)) rename_map <- c(rename_map, stats::setNames(param_var, "PARAM"))
  if (paramn_var %in% names(lookup)) rename_map <- c(rename_map, stats::setNames(paramn_var, "PARAMN"))
  names(lookup)[match(rename_map, names(lookup))] <- names(rename_map)

  if (!is.null(keep_paramcd)) {
    lookup <- lookup[lookup$PARAMCD %in% keep_paramcd, , drop = FALSE]
  }

  rownames(lookup) <- NULL
  lookup
}
