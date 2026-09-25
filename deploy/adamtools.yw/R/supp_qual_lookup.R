#' Extract selected SUPPQUAL qualifiers as a wide lookup table
#'
#' Extracts selected `QNAM` values from a `SUPP--` dataset, converts
#' `IDVARVAL` to the parent domain's sequence key, and pivots to a wide
#' lookup table ready to join onto the parent dataset. This complements
#' (does not replace) `metatools::combine_supp()`: use `combine_supp()` for
#' a full parent+SUPP merge, and this function when only a filtered/wide
#' subset of qualifiers is needed for an ad hoc join, without the full
#' `combine_supp()` merge semantics.
#'
#' @param supp A `SUPP--` data frame or tibble (with at least `USUBJID`,
#'   `IDVAR`, `IDVARVAL`, `QNAM`, `QVAL`).
#' @param qnam Optional character vector of `QNAM` values to keep. If `NULL`
#'   (default), all `QNAM` values in `supp` are kept.
#' @param idvar_to Name of the new column to store the converted
#'   `IDVARVAL`, matching the parent domain's sequence variable (e.g.
#'   `"AESEQ"`). If `NULL` (default), `IDVARVAL` is kept as-is (character).
#' @param idvarval_type Type to coerce `IDVARVAL` to before renaming/joining;
#'   `"numeric"` (default) or `"character"`.
#' @param keys Character vector of subject-level key variables to retain in
#'   the output alongside the pivoted qualifiers. Defaults to
#'   `c("STUDYID", "USUBJID")`.
#' @param rename Optional named character vector to rename pivoted `QNAM`
#'   columns after pivoting, e.g. `c(COMMENT = "AECOMM")` renames the `QNAM
#'   == "COMMENT"` column to `AECOMM`.
#'
#' @return A wide data frame with one row per `keys` + `idvar_to` (or
#'   `IDVARVAL`) combination and one column per retained `QNAM`.
#' @export
supp_qual_lookup <- function(supp,
                             qnam = NULL,
                             idvar_to = NULL,
                             idvarval_type = c("numeric", "character"),
                             keys = c("STUDYID", "USUBJID"),
                             rename = NULL) {
  idvarval_type <- match.arg(idvarval_type)

  if (!inherits(supp, "data.frame")) {
    stop("`supp` must be a data frame or tibble.", call. = FALSE)
  }
  required_cols <- c("IDVARVAL", "QNAM", "QVAL")
  missing_cols <- setdiff(required_cols, names(supp))
  if (length(missing_cols) > 0L) {
    stop(
      "`supp` is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  keys <- intersect(keys, names(supp))

  result <- supp
  if (!is.null(qnam)) {
    result <- result[result$QNAM %in% qnam, , drop = FALSE]
  }

  result$IDVARVAL <- if (idvarval_type == "numeric") {
    as.numeric(result$IDVARVAL)
  } else {
    as.character(result$IDVARVAL)
  }

  if (!is.null(idvar_to)) {
    names(result)[names(result) == "IDVARVAL"] <- idvar_to
  }

  id_col <- if (!is.null(idvar_to)) idvar_to else "IDVARVAL"
  select_cols <- unique(c(keys, id_col, "QNAM", "QVAL"))
  result <- result[, intersect(select_cols, names(result)), drop = FALSE]

  wide <- tidyr::pivot_wider(
    result,
    id_cols = dplyr::all_of(intersect(c(keys, id_col), names(result))),
    names_from = "QNAM",
    values_from = "QVAL"
  )

  if (!is.null(rename)) {
    matched <- intersect(names(rename), names(wide))
    for (nm in matched) {
      names(wide)[names(wide) == nm] <- rename[[nm]]
    }
  }

  wide
}
