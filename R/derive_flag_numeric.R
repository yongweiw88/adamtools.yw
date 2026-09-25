#' Derive numeric counterparts for character Y/N-style flags
#'
#' Creates one or more numeric flag variables (e.g. `SAFFN` from `SAFFL`,
#' `FASFN` from `FASFL`) from existing character flag variables, applying a
#' consistent yes/no/missing numeric encoding across multiple mappings in a
#' single call. This formalizes the repeated `if_else(SAFFL == "Y", 1, 0)`
#' -style pattern seen across ADSL and BDS criteria-flag derivations.
#'
#' @param dataset A data frame or tibble.
#' @param mappings A named character vector mapping existing flag variable
#'   names to new numeric variable names, e.g.
#'   `c(SAFFL = "SAFFN", FASFL = "FASFN")`.
#' @param yes Value in the source flag variable(s) treated as "yes".
#'   Defaults to `"Y"`.
#' @param no Numeric value assigned when the source flag is present but not
#'   equal to `yes` (e.g. `"N"`). Defaults to `0`.
#' @param missing Numeric value assigned when the source flag is `NA`.
#'   Defaults to `NA_real_`.
#'
#' @return `dataset` with the new numeric flag variable(s) added.
#' @export
derive_flag_numeric <- function(dataset,
                                mappings,
                                yes = "Y",
                                no = 0,
                                missing = NA_real_) {
  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }
  if (is.null(names(mappings)) || any(!nzchar(names(mappings)))) {
    stop("`mappings` must be a named character vector, e.g. c(SAFFL = \"SAFFN\").", call. = FALSE)
  }

  source_vars <- names(mappings)
  missing_vars <- setdiff(source_vars, names(dataset))
  if (length(missing_vars) > 0L) {
    stop(
      "The following source flag variable(s) are not in `dataset`: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }

  result <- dataset
  for (i in seq_along(mappings)) {
    src <- source_vars[i]
    dest <- mappings[[i]]
    src_vals <- result[[src]]
    mapped <- ifelse(is.na(src_vals), missing, ifelse(src_vals == yes, 1, no))
    result[[dest]] <- mapped
  }

  result
}
