#' Derive PARAMCD (or another key) from a compound-key lookup table
#'
#' Joins `dataset` to a reference lookup table (e.g. an `advsparm`-style
#' dataset mapping `VSTESTCD`/`VSPOS` to `PARAMCD`) on one or more shared key
#' columns, generalizing [apply_lookup()] to compound (multi-column) keys.
#' Blank strings and `NA` are treated as the same join key on both sides by
#' default, since source SDTM key columns (e.g. `VSPOS`) commonly use blank
#' for "not applicable" while a lookup table may use `NA` (or vice versa).
#' Unmatched rows fall back to a caller-supplied column (e.g. the original
#' test code) rather than erroring, with a message reporting the fallback
#' count so the caller can decide whether to investigate further.
#'
#' @param dataset A data frame or tibble to derive the new key onto.
#' @param lookup A reference data frame containing `by` and `value_var`.
#' @param by Character vector of column names present in both `dataset` and
#'   `lookup` to join on (e.g. `c("VSTESTCD", "VSPOS")`).
#' @param value_var Name of the column in `lookup` holding the value to bring
#'   over (e.g. `"PARAMCD"`).
#' @param new_var Name of the new column to create in `dataset`. Defaults to
#'   `value_var`.
#' @param fallback_var Optional name of a column in `dataset` to use as
#'   `new_var` when no `lookup` match is found (e.g. `"VSTESTCD"` to fall
#'   back to a direct rename). If `NULL` (default), unmatched rows get `NA`.
#' @param blank_as_na Logical; treat blank strings (`""`) as equivalent to
#'   `NA` in the `by` columns on both sides before joining. Defaults to
#'   `TRUE`.
#' @param quiet Logical; suppress the unmatched-row count message. Defaults
#'   to `FALSE`.
#'
#' @return `dataset` with `new_var` added.
#' @export
derive_paramcd_from_lookup <- function(dataset,
                                       lookup,
                                       by,
                                       value_var,
                                       new_var = value_var,
                                       fallback_var = NULL,
                                       blank_as_na = TRUE,
                                       quiet = FALSE) {
  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }
  if (!inherits(lookup, "data.frame")) {
    stop("`lookup` must be a data frame or tibble.", call. = FALSE)
  }
  .adamtools_check_character_vector(by, "by", allow_null = FALSE)
  missing_in_dataset <- setdiff(by, names(dataset))
  if (length(missing_in_dataset) > 0L) {
    stop("`by` column(s) not found in `dataset`: ", paste(missing_in_dataset, collapse = ", "), call. = FALSE)
  }
  missing_in_lookup <- setdiff(c(by, value_var), names(lookup))
  if (length(missing_in_lookup) > 0L) {
    stop("`by`/`value_var` column(s) not found in `lookup`: ", paste(missing_in_lookup, collapse = ", "), call. = FALSE)
  }
  if (!is.null(fallback_var) && !fallback_var %in% names(dataset)) {
    stop("`fallback_var` is not a column in `dataset`.", call. = FALSE)
  }

  join_key <- function(x) {
    x <- as.character(x)
    if (blank_as_na) x[!is.na(x) & x == ""] <- NA_character_
    x
  }

  data_keys <- do.call(paste, c(lapply(dataset[by], join_key), sep = ""))
  lookup_norm <- unique(lookup[c(by, value_var)])
  lookup_keys <- do.call(paste, c(lapply(lookup_norm[by], join_key), sep = ""))

  if (anyDuplicated(lookup_keys) > 0L) {
    stop("`lookup` has duplicate rows for the same `by` key combination; deduplicate before calling.", call. = FALSE)
  }

  matched <- unname(stats::setNames(lookup_norm[[value_var]], lookup_keys)[data_keys])

  n_unmatched <- sum(is.na(matched))
  if (n_unmatched > 0L && !quiet) {
    message(
      "derive_paramcd_from_lookup(): ", n_unmatched, " of ", nrow(dataset),
      " row(s) had no `lookup` match on ", paste(by, collapse = "/"),
      if (!is.null(fallback_var)) paste0("; falling back to `", fallback_var, "`.") else "."
    )
  }

  if (!is.null(fallback_var)) {
    matched <- ifelse(is.na(matched), as.character(dataset[[fallback_var]]), matched)
  }

  dataset[[new_var]] <- matched
  dataset
}
