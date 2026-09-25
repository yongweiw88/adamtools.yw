#' Apply a value lookup/recode to a variable
#'
#' Maps values in a variable to new values using a lookup table. This is a
#' generic recode helper for cases where a simple key-value mapping is needed
#' and no `metacore` specification object is available (e.g. `country_code_lookup`) or a
#' dataset-to-dataset merge (`admiral::derive_vars_merged_lookup()`) does not apply.
#' Prefer `metatools::create_var_from_codelist()` when a `metacore` spec is
#' available, and `admiral::derive_vars_merged_lookup()` for merging a full
#' lookup dataset by key variables.
#'
#' @param dataset A data frame or tibble.
#' @param var Name of the variable in `dataset` to look up (unquoted).
#' @param new_var Name of the new variable to create with mapped values
#'   (unquoted).
#' @param lookup A named character vector (`c(key = "value", ...)`) or a
#'   two-column data frame/tibble giving the key-to-value mapping. When a data
#'   frame is supplied, the first column is treated as the key and the second
#'   column as the value.
#' @param unmatched Value to use when a key in `var` has no match in `lookup`.
#'   Defaults to `NA_character_`. Set to `"error"` to stop with an error
#'   listing the unmatched keys instead.
#'
#' @return The input dataset with the new mapped variable added.
#' @export
apply_lookup <- function(dataset, var, new_var, lookup, unmatched = NA_character_) {
  var_sym <- rlang::ensym(var)
  new_var_sym <- rlang::ensym(new_var)

  var_name <- rlang::as_name(var_sym)
  new_var_name <- rlang::as_name(new_var_sym)

  if (!var_name %in% names(dataset)) {
    stop("`var` is not a column in `dataset`.", call. = FALSE)
  }

  if (is.data.frame(lookup)) {
    if (ncol(lookup) < 2) {
      stop("`lookup` must have at least two columns when supplied as a data frame.", call. = FALSE)
    }
    keys <- as.character(lookup[[1]])
    values <- lookup[[2]]
    lookup_vec <- stats::setNames(values, keys)
  } else if (is.vector(lookup) && !is.null(names(lookup))) {
    lookup_vec <- lookup
  } else {
    stop("`lookup` must be a named vector or a two-column data frame/tibble.", call. = FALSE)
  }

  input_keys <- as.character(dataset[[var_name]])
  mapped <- unname(lookup_vec[input_keys])

  is_unmatched <- !is.na(input_keys) & !(input_keys %in% names(lookup_vec))

  if (identical(unmatched, "error")) {
    if (any(is_unmatched)) {
      bad_keys <- unique(input_keys[is_unmatched])
      stop(
        "The following values in `", var_name, "` have no match in `lookup`: ",
        paste(bad_keys, collapse = ", "),
        call. = FALSE
      )
    }
  } else {
    mapped[is_unmatched] <- unmatched
  }

  dataset[[new_var_name]] <- mapped
  dataset
}
