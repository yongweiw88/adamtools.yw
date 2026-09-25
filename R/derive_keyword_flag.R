#' Flag records where a text field contains a keyword
#'
#' Sets a `"Y"`/`""` flag when any of `text_vars` contains `keyword`
#' (case-insensitive substring match by default). This formalizes the
#' repeated `if_else(str_detect(coalesce(x, ""), regex(keyword,
#' ignore_case=TRUE)), "Y", "")` pattern seen across condition-specific
#' analysis flags (e.g. `MHDEPFL`/`MHANXFL`/`MHSCDFL` matching "DEPRESSION"/
#' "ANXIETY"/"SUICIDE" in a reported-term field, or a customized-query flag
#' matching "preg" across several MedDRA hierarchy columns), without
#' hardcoding any study-specific keyword or column list.
#'
#' @param dataset A data frame or tibble.
#' @param text_vars Character vector of column names to search. A record is
#'   flagged if `keyword` is found in *any* of them.
#' @param keyword Keyword or regular expression to search for.
#' @param flag_var Name of the new flag column to create.
#' @param ignore_case Logical; case-insensitive match. Defaults to `TRUE`.
#' @param fixed Logical; treat `keyword` as a literal substring rather than
#'   a regular expression. Defaults to `TRUE` (the common case for a plain
#'   word like "DEPRESSION"); set to `FALSE` to use `keyword` as a regex.
#' @param true_value,false_value Values assigned when `keyword` is found /
#'   not found. Default to `"Y"`/`""`.
#'
#' @return `dataset` with `flag_var` added.
#' @export
derive_keyword_flag <- function(dataset,
                                text_vars,
                                keyword,
                                flag_var,
                                ignore_case = TRUE,
                                fixed = TRUE,
                                true_value = "Y",
                                false_value = "") {
  .adamtools_check_data_frame(dataset)
  .adamtools_check_character_vector(text_vars, "text_vars", allow_null = FALSE)
  .adamtools_check_scalar_name(flag_var, "flag_var")
  .adamtools_check_columns(dataset, text_vars)
  if (!is.character(keyword) || length(keyword) != 1L || is.na(keyword) || identical(keyword, "")) {
    stop("`keyword` must be a single non-missing character value.", call. = FALSE)
  }

  pattern <- if (fixed) {
    stringr::fixed(keyword, ignore_case = ignore_case)
  } else {
    stringr::regex(keyword, ignore_case = ignore_case)
  }

  hits <- lapply(text_vars, function(v) {
    stringr::str_detect(dplyr::coalesce(as.character(dataset[[v]]), ""), pattern)
  })
  any_hit <- Reduce(`|`, hits)

  dataset[[flag_var]] <- ifelse(any_hit, true_value, false_value)
  dataset
}
