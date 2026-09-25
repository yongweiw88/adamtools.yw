#' Categorize HIV drug-resistance results from caller-supplied rules
#'
#' Categorizes HIV resistance-related values using a caller-supplied rule table.
#' The function supports numeric fold-change intervals, categorical source
#' results, or rules that combine both, optionally matched by drug, assay,
#' reference system, or any other caller-selected columns. It intentionally
#' contains no drug abbreviations, drug classes, mutation lists, Stanford/USIAS
#' cutoffs, phenotype boundaries, or category labels.
#'
#' Numeric rules are defined by the columns named by `lower_var` and
#' `upper_var`; missing lower or upper bounds are treated as open-ended.
#' Boundaries are inclusive unless rule-level logical columns are supplied via
#' `lower_inclusive_var` and/or `upper_inclusive_var`. Categorical rules are
#' defined by the column named by `source_value_var`. If a row matches no rules,
#' `unmatched = "error"` stops. If a row matches multiple rules, the function
#' always stops so overlapping intervals or duplicate lookups are not silently
#' resolved.
#'
#' @param dataset Data frame with resistance-related source values.
#' @param rules Rule table. Must contain the columns named by
#'   `category_rule_var` and `ordinal_rule_var`, all `match_vars`, and at least
#'   one usable numeric-bound or categorical-source rule column.
#' @param value_var Optional numeric source value column in `dataset`, such as a
#'   fold-change value.
#' @param source_result_var Optional categorical source result column in
#'   `dataset`, such as an interpreted susceptibility result.
#' @param match_vars Optional character vector of columns that must match
#'   exactly between `dataset` and `rules` before value rules are applied (for
#'   example, drug, assay, or reference-system variables).
#' @param lower_var Rule-table lower numeric boundary column. Defaults to
#'   `"LOW"`.
#' @param upper_var Rule-table upper numeric boundary column. Defaults to
#'   `"HIGH"`.
#' @param lower_inclusive_var Optional rule-table logical column controlling
#'   lower-bound inclusivity per numeric rule. If `NULL`, lower bounds are
#'   inclusive.
#' @param upper_inclusive_var Optional rule-table logical column controlling
#'   upper-bound inclusivity per numeric rule. If `NULL`, upper bounds are
#'   inclusive.
#' @param source_value_var Rule-table categorical source-value column. Defaults
#'   to `"SOURCE_VALUE"`.
#' @param category_rule_var Rule-table category label column. Defaults to
#'   `"CATEGORY"`.
#' @param ordinal_rule_var Rule-table category ordinal column. Defaults to
#'   `"CATEGORYN"`.
#' @param category_var Output category label column. Defaults to `"RESCAT"`.
#' @param ordinal_var Output category ordinal column. Defaults to `"RESCATN"`.
#' @param unmatched Behavior for rows that match no rule: `"error"` (default)
#'   or `"na"`.
#' @param overwrite Logical; allow output columns to replace existing columns.
#'   Defaults to `FALSE`.
#'
#' @return `dataset` with `category_var` and `ordinal_var` added.
#' @examples
#' rules <- data.frame(
#'   DRUG = "CALLER_DRUG",
#'   LOW = c(0, 1),
#'   HIGH = c(1, NA),
#'   HIGH_INCL = c(TRUE, FALSE),
#'   CATEGORY = c("Category 1", "Category 2"),
#'   CATEGORYN = c(1, 2)
#' )
#' @export
derive_hiv_resistance_category <- function(dataset,
                                           rules,
                                           value_var = NULL,
                                           source_result_var = NULL,
                                           match_vars = NULL,
                                           lower_var = "LOW",
                                           upper_var = "HIGH",
                                           lower_inclusive_var = NULL,
                                           upper_inclusive_var = NULL,
                                           source_value_var = "SOURCE_VALUE",
                                           category_rule_var = "CATEGORY",
                                           ordinal_rule_var = "CATEGORYN",
                                           category_var = "RESCAT",
                                           ordinal_var = "RESCATN",
                                           unmatched = c("error", "na"),
                                           overwrite = FALSE) {
  unmatched <- match.arg(unmatched)
  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }
  if (!inherits(rules, "data.frame")) {
    stop("`rules` must be a data frame or tibble.", call. = FALSE)
  }
  if (is.null(value_var) && is.null(source_result_var)) {
    stop("At least one of `value_var` or `source_result_var` must be supplied.", call. = FALSE)
  }
  if (nrow(rules) == 0L) {
    stop("`rules` must contain at least one row.", call. = FALSE)
  }

  required_dataset <- c(match_vars, value_var, source_result_var)
  missing_dataset <- setdiff(required_dataset[!is.null(required_dataset)], names(dataset))
  if (length(missing_dataset) > 0L) {
    stop(
      "`dataset` is missing required column(s): ",
      paste(missing_dataset, collapse = ", "),
      call. = FALSE
    )
  }
  required_rules <- c(match_vars, category_rule_var, ordinal_rule_var)
  missing_rules <- setdiff(required_rules, names(rules))
  if (length(missing_rules) > 0L) {
    stop(
      "`rules` is missing required column(s): ",
      paste(missing_rules, collapse = ", "),
      call. = FALSE
    )
  }
  if (!overwrite) {
    existing_outputs <- intersect(c(category_var, ordinal_var), names(dataset))
    if (length(existing_outputs) > 0L) {
      stop(
        "Output column(s) already exist in `dataset`: ",
        paste(existing_outputs, collapse = ", "),
        ". Set `overwrite = TRUE` to replace them.",
        call. = FALSE
      )
    }
  }
  if (!is.null(value_var) && !is.numeric(dataset[[value_var]])) {
    stop("`value_var` must identify a numeric column.", call. = FALSE)
  }

  has_numeric_rule <- lower_var %in% names(rules) || upper_var %in% names(rules)
  has_categorical_rule <- source_value_var %in% names(rules)
  if (!has_numeric_rule && !has_categorical_rule) {
    stop(
      "`rules` must contain numeric bound columns or the categorical `source_value_var` column.",
      call. = FALSE
    )
  }
  if (has_numeric_rule && is.null(value_var)) {
    stop("`value_var` is required when `rules` contains numeric bound columns.", call. = FALSE)
  }
  if (has_categorical_rule && is.null(source_result_var) &&
      any(!is.na(rules[[source_value_var]]) & rules[[source_value_var]] != "")) {
    stop(
      "`source_result_var` is required when `rules` contains categorical source values.",
      call. = FALSE
    )
  }

  if (has_numeric_rule) {
    if (lower_var %in% names(rules) && !is.numeric(rules[[lower_var]])) {
      stop("`lower_var` must identify a numeric rule column.", call. = FALSE)
    }
    if (upper_var %in% names(rules) && !is.numeric(rules[[upper_var]])) {
      stop("`upper_var` must identify a numeric rule column.", call. = FALSE)
    }
  }
  if (!is.null(lower_inclusive_var)) {
    .hiv_res_check_logical_rule(rules, lower_inclusive_var)
  }
  if (!is.null(upper_inclusive_var)) {
    .hiv_res_check_logical_rule(rules, upper_inclusive_var)
  }

  out_category <- rep(NA_character_, nrow(dataset))
  out_ordinal <- rep(NA, nrow(dataset))
  unmatched_rows <- integer(0)

  for (i in seq_len(nrow(dataset))) {
    candidate <- rep(TRUE, nrow(rules))

    for (match_var in match_vars) {
      candidate <- candidate &
        !is.na(dataset[[match_var]][i]) &
        !is.na(rules[[match_var]]) &
        as.character(rules[[match_var]]) == as.character(dataset[[match_var]][i])
    }

    rule_has_condition <- rep(FALSE, nrow(rules))

    if (has_categorical_rule) {
      categorical_rule <- !is.na(rules[[source_value_var]]) & rules[[source_value_var]] != ""
      rule_has_condition <- rule_has_condition | categorical_rule
      if (!is.null(source_result_var)) {
        candidate[categorical_rule] <- candidate[categorical_rule] &
          !is.na(dataset[[source_result_var]][i]) &
          as.character(rules[[source_value_var]][categorical_rule]) ==
            as.character(dataset[[source_result_var]][i])
      } else {
        candidate[categorical_rule] <- FALSE
      }
    }

    if (has_numeric_rule) {
      lower <- if (lower_var %in% names(rules)) rules[[lower_var]] else rep(NA_real_, nrow(rules))
      upper <- if (upper_var %in% names(rules)) rules[[upper_var]] else rep(NA_real_, nrow(rules))
      numeric_rule <- !is.na(lower) | !is.na(upper)
      rule_has_condition <- rule_has_condition | numeric_rule

      value <- dataset[[value_var]][i]
      numeric_match <- rep(FALSE, nrow(rules))
      if (!is.na(value)) {
        lower_inclusive <- .hiv_res_inclusive_values(rules, lower_inclusive_var, TRUE)
        upper_inclusive <- .hiv_res_inclusive_values(rules, upper_inclusive_var, TRUE)
        lower_ok <- is.na(lower) |
          ifelse(lower_inclusive, value >= lower, value > lower)
        upper_ok <- is.na(upper) |
          ifelse(upper_inclusive, value <= upper, value < upper)
        numeric_match <- numeric_rule & lower_ok & upper_ok
      }
      candidate[numeric_rule] <- candidate[numeric_rule] & numeric_match[numeric_rule]
    }

    candidate[!rule_has_condition] <- FALSE
    matched <- which(candidate)
    if (length(matched) > 1L) {
      stop(
        "`dataset` row ", i, " matches multiple resistance categorization rules.",
        call. = FALSE
      )
    }
    if (length(matched) == 0L) {
      unmatched_rows <- c(unmatched_rows, i)
    } else {
      out_category[i] <- as.character(rules[[category_rule_var]][matched])
      out_ordinal[i] <- rules[[ordinal_rule_var]][matched]
    }
  }

  if (length(unmatched_rows) > 0L && unmatched == "error") {
    stop(
      length(unmatched_rows),
      " row(s) did not match any resistance categorization rule; first row(s): ",
      paste(utils::head(unmatched_rows, 10L), collapse = ", "),
      call. = FALSE
    )
  }

  result <- dataset
  result[[category_var]] <- out_category
  result[[ordinal_var]] <- out_ordinal
  result
}

.hiv_res_check_logical_rule <- function(rules, column) {
  if (!column %in% names(rules)) {
    stop("`rules` is missing logical boundary column `", column, "`.", call. = FALSE)
  }
  if (!is.logical(rules[[column]]) || any(is.na(rules[[column]]))) {
    stop("`", column, "` must be a non-missing logical rule column.", call. = FALSE)
  }
}

.hiv_res_inclusive_values <- function(rules, column, default) {
  if (is.null(column)) {
    return(rep(default, nrow(rules)))
  }
  rules[[column]]
}
