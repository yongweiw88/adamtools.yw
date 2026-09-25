#' Derive table-driven ISR source and aggregate event records
#'
#' Builds an ISR-style adverse-event analysis input by retaining caller-selected
#' source AE records and, optionally, appending derived aggregate event records
#' from a rule table. The function is deliberately term- and study-agnostic:
#' it does not contain any MedDRA preferred terms, treatment names, category
#' labels, visit labels, order values, or output labels. Callers must prepare
#' any ISR/infusion flags or categories before calling this helper and must
#' provide any aggregate definitions explicitly in `aggregate_rules`.
#'
#' Each row in `aggregate_rules` represents a source-to-derived copy rule. The
#' required rule columns are `rule_source_term_var` and
#' `rule_derived_term_var`. Optional source-category, derived-category, order,
#' and DTYPE rule columns are used when present. A missing (`NA`) source term or
#' source category in a rule is treated as a wildcard, allowing callers to
#' create records such as an "overall" aggregate without enumerating every
#' source term. Multiple different rules can match the same source row (for
#' example one overall rule plus one grouped-term rule), but exact duplicate
#' rule definitions are rejected because they would silently duplicate output
#' rows.
#'
#' @param dataset AE analysis input data frame.
#' @param term_var Name of the source/output event term column, for example
#'   `"AEDECOD"`.
#' @param category_var Optional name of the source/output event category column,
#'   for example `"AECAT"`. Required when rules use source or derived category
#'   columns.
#' @param qualifier_var Optional caller-prepared flag or categorization column
#'   used to select source rows, for example an ISR flag. If `NULL`, all rows
#'   are eligible before `category_values` filtering.
#' @param qualifier_values Values of `qualifier_var` that identify source rows
#'   to retain. Defaults to `"Y"`.
#' @param category_values Optional values of `category_var` to retain. Leave
#'   `NULL` to avoid category filtering.
#' @param aggregate_rules Optional data frame of aggregate-copy rules. Required
#'   columns default to `source_term` and `derived_term`; optional columns
#'   default to `source_category`, `derived_category`, `derived_order`, and
#'   `dtype` when present.
#' @param rule_source_term_var Column in `aggregate_rules` containing source
#'   terms to match. `NA` is a wildcard. Defaults to `"source_term"`.
#' @param rule_source_category_var Optional column in `aggregate_rules`
#'   containing source categories to match. `NA` is a wildcard. Defaults to
#'   `"source_category"` when that column exists.
#' @param rule_derived_term_var Column in `aggregate_rules` containing the
#'   derived output term. Defaults to `"derived_term"`.
#' @param rule_derived_category_var Optional column in `aggregate_rules`
#'   containing the derived output category. Defaults to `"derived_category"`
#'   when that column exists.
#' @param rule_derived_order_var Optional column in `aggregate_rules`
#'   containing the derived output order value. Defaults to `"derived_order"`
#'   when that column exists.
#' @param rule_dtype_var Optional column in `aggregate_rules` containing the
#'   derived `DTYPE` value. Defaults to `"dtype"` when that column exists.
#' @param order_var Optional name of the output order column to update for
#'   derived rows, for example `"AEDECODN"`.
#' @param dtype_var Name of the output derivation-type column. Created when
#'   absent. Defaults to `"DTYPE"`.
#' @param derived_dtype Default value assigned to `dtype_var` for derived rows
#'   when `rule_dtype_var` is not present or is missing for a rule. Defaults to
#'   `"DERIVED"`.
#' @param keep_source_records Logical; keep qualifying source records in the
#'   returned data. Defaults to `TRUE`. Set to `FALSE` to return only derived
#'   aggregate rows.
#' @param unmatched_terms Policy for qualifying source records that do not
#'   match any aggregate rule when `aggregate_rules` is supplied. `"ignore"`
#'   keeps the rows without requiring a derived record; `"error"` stops and
#'   lists examples. Defaults to `"ignore"`.
#' @param unmatched_rules Policy for aggregate rules that match no qualifying
#'   source row. `"error"` stops; `"ignore"` allows empty rules. Defaults to
#'   `"error"`.
#'
#' @return A data frame with qualifying source rows followed by any derived
#'   aggregate rows. The return grain is one retained source AE row plus one
#'   copy for each matching aggregate rule.
#' @export
derive_isr_events <- function(dataset,
                              term_var,
                              category_var = NULL,
                              qualifier_var = NULL,
                              qualifier_values = "Y",
                              category_values = NULL,
                              aggregate_rules = NULL,
                              rule_source_term_var = "source_term",
                              rule_source_category_var = "source_category",
                              rule_derived_term_var = "derived_term",
                              rule_derived_category_var = "derived_category",
                              rule_derived_order_var = "derived_order",
                              rule_dtype_var = "dtype",
                              order_var = NULL,
                              dtype_var = "DTYPE",
                              derived_dtype = "DERIVED",
                              keep_source_records = TRUE,
                              unmatched_terms = c("ignore", "error"),
                              unmatched_rules = c("error", "ignore")) {
  .adamtools_check_data_frame(dataset)
  .adamtools_check_scalar_name(term_var, "term_var")
  .adamtools_check_scalar_name(category_var, "category_var", allow_null = TRUE)
  .adamtools_check_scalar_name(qualifier_var, "qualifier_var", allow_null = TRUE)
  .adamtools_check_scalar_name(order_var, "order_var", allow_null = TRUE)
  .adamtools_check_scalar_name(dtype_var, "dtype_var")
  .adamtools_check_columns(dataset, c(term_var, category_var, qualifier_var), "dataset")

  unmatched_terms <- match.arg(unmatched_terms)
  unmatched_rules <- match.arg(unmatched_rules)
  if (!is.logical(keep_source_records) || length(keep_source_records) != 1L || is.na(keep_source_records)) {
    stop("`keep_source_records` must be a single TRUE/FALSE value.", call. = FALSE)
  }

  output_names <- c(term_var, category_var, order_var, dtype_var)
  duplicate_outputs <- unique(output_names[!is.na(output_names) & duplicated(output_names)])
  if (length(duplicate_outputs) > 0L) {
    stop(
      "Output column names must be unique; duplicate name(s): ",
      paste(duplicate_outputs, collapse = ", "),
      call. = FALSE
    )
  }

  qualifies <- rep(TRUE, nrow(dataset))
  if (!is.null(qualifier_var)) {
    qualifies <- qualifies & !is.na(dataset[[qualifier_var]]) & dataset[[qualifier_var]] %in% qualifier_values
  }
  if (!is.null(category_values)) {
    if (is.null(category_var)) {
      stop("`category_var` is required when `category_values` is supplied.", call. = FALSE)
    }
    qualifies <- qualifies & !is.na(dataset[[category_var]]) & dataset[[category_var]] %in% category_values
  }

  source <- dataset[qualifies, , drop = FALSE]
  if (!dtype_var %in% names(source)) {
    source[[dtype_var]] <- rep(NA_character_, nrow(source))
  }
  if (!is.null(order_var) && !order_var %in% names(source)) {
    source[[order_var]] <- rep(NA, nrow(source))
  }
  if (is.factor(source[[term_var]])) {
    source[[term_var]] <- as.character(source[[term_var]])
  }
  if (!is.null(category_var) && is.factor(source[[category_var]])) {
    source[[category_var]] <- as.character(source[[category_var]])
  }
  if (is.factor(source[[dtype_var]])) {
    source[[dtype_var]] <- as.character(source[[dtype_var]])
  }

  if (is.null(aggregate_rules)) {
    if (keep_source_records) {
      rownames(source) <- NULL
      return(source)
    }
    return(source[0L, , drop = FALSE])
  }
  .adamtools_check_data_frame(aggregate_rules, "aggregate_rules")
  .adamtools_check_scalar_name(rule_source_term_var, "rule_source_term_var")
  .adamtools_check_scalar_name(rule_source_category_var, "rule_source_category_var", allow_null = TRUE)
  .adamtools_check_scalar_name(rule_derived_term_var, "rule_derived_term_var")
  .adamtools_check_scalar_name(rule_derived_category_var, "rule_derived_category_var", allow_null = TRUE)
  .adamtools_check_scalar_name(rule_derived_order_var, "rule_derived_order_var", allow_null = TRUE)
  .adamtools_check_scalar_name(rule_dtype_var, "rule_dtype_var", allow_null = TRUE)
  .adamtools_check_columns(aggregate_rules, c(rule_source_term_var, rule_derived_term_var), "aggregate_rules")

  if (nrow(aggregate_rules) == 0L) {
    if (keep_source_records) {
      rownames(source) <- NULL
      return(source)
    }
    return(source[0L, , drop = FALSE])
  }

  has_source_category_rule <- !is.null(rule_source_category_var) &&
    rule_source_category_var %in% names(aggregate_rules)
  has_derived_category_rule <- !is.null(rule_derived_category_var) &&
    rule_derived_category_var %in% names(aggregate_rules)
  has_order_rule <- !is.null(rule_derived_order_var) &&
    rule_derived_order_var %in% names(aggregate_rules)
  has_dtype_rule <- !is.null(rule_dtype_var) &&
    rule_dtype_var %in% names(aggregate_rules)

  if ((has_source_category_rule || has_derived_category_rule) && is.null(category_var)) {
    stop("`category_var` is required when aggregate rules use category columns.", call. = FALSE)
  }
  if (has_order_rule && is.null(order_var)) {
    stop("`order_var` is required when aggregate rules use an order column.", call. = FALSE)
  }

  duplicate_rule_cols <- c(
    rule_source_term_var,
    if (has_source_category_rule) rule_source_category_var else NULL,
    rule_derived_term_var,
    if (has_derived_category_rule) rule_derived_category_var else NULL
  )
  duplicate_rule_rows <- duplicated(aggregate_rules[duplicate_rule_cols]) |
    duplicated(aggregate_rules[duplicate_rule_cols], fromLast = TRUE)
  if (any(duplicate_rule_rows)) {
    examples <- .adamtools_format_key_values(aggregate_rules[duplicate_rule_rows, , drop = FALSE], duplicate_rule_cols)
    stop("`aggregate_rules` contains duplicate aggregate rule definitions: ", examples, call. = FALSE)
  }

  rule_matches <- vector("list", nrow(aggregate_rules))
  source_matched <- rep(FALSE, nrow(source))
  for (i in seq_len(nrow(aggregate_rules))) {
    matched <- .adamtools_match_isr_rule(
      source = source,
      term_var = term_var,
      category_var = category_var,
      rule = aggregate_rules[i, , drop = FALSE],
      rule_source_term_var = rule_source_term_var,
      rule_source_category_var = if (has_source_category_rule) rule_source_category_var else NULL
    )
    rule_matches[[i]] <- matched
    source_matched <- source_matched | matched
  }

  empty_rules <- vapply(rule_matches, function(x) !any(x), logical(1L))
  if (any(empty_rules) && identical(unmatched_rules, "error")) {
    examples <- .adamtools_format_key_values(
      aggregate_rules[empty_rules, , drop = FALSE],
      c(rule_source_term_var, if (has_source_category_rule) rule_source_category_var else NULL, rule_derived_term_var)
    )
    stop("The following `aggregate_rules` did not match any qualifying source row: ", examples, call. = FALSE)
  }

  if (any(!source_matched) && identical(unmatched_terms, "error")) {
    examples <- .adamtools_format_key_values(
      source[!source_matched, , drop = FALSE],
      c(term_var, category_var)
    )
    stop("Some qualifying source rows did not match any aggregate rule: ", examples, call. = FALSE)
  }

  derived_list <- vector("list", nrow(aggregate_rules))
  for (i in seq_len(nrow(aggregate_rules))) {
    matched <- rule_matches[[i]]
    if (!any(matched)) {
      derived_list[[i]] <- source[0L, , drop = FALSE]
      next
    }

    rule <- aggregate_rules[i, , drop = FALSE]
    derived <- source[matched, , drop = FALSE]
    derived[[term_var]] <- rule[[rule_derived_term_var]][[1L]]
    if (has_derived_category_rule) {
      derived[[category_var]] <- rule[[rule_derived_category_var]][[1L]]
    }
    if (has_order_rule) {
      derived[[order_var]] <- rule[[rule_derived_order_var]][[1L]]
    }
    dtype_value <- derived_dtype
    if (has_dtype_rule && !is.na(rule[[rule_dtype_var]][[1L]])) {
      dtype_value <- rule[[rule_dtype_var]][[1L]]
    }
    derived[[dtype_var]] <- dtype_value
    derived_list[[i]] <- derived
  }

  derived_rows <- dplyr::bind_rows(derived_list)
  result <- if (keep_source_records) {
    dplyr::bind_rows(source, derived_rows)
  } else {
    derived_rows
  }
  rownames(result) <- NULL
  result
}

.adamtools_match_isr_rule <- function(source,
                                      term_var,
                                      category_var,
                                      rule,
                                      rule_source_term_var,
                                      rule_source_category_var = NULL) {
  matched <- rep(TRUE, nrow(source))
  source_term <- rule[[rule_source_term_var]][[1L]]
  if (!is.na(source_term)) {
    matched <- matched & !is.na(source[[term_var]]) & as.character(source[[term_var]]) == as.character(source_term)
  }

  if (!is.null(rule_source_category_var)) {
    source_category <- rule[[rule_source_category_var]][[1L]]
    if (!is.na(source_category)) {
      matched <- matched &
        !is.na(source[[category_var]]) &
        as.character(source[[category_var]]) == as.character(source_category)
    }
  }
  matched
}
