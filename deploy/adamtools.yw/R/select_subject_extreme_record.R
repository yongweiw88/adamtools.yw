#' Select the first or last qualifying record within subject-level groups
#'
#' Selects one deterministic record per subject (or per caller-supplied group)
#' after the caller has applied study-specific qualification and source
#' precedence rules. This captures the common ADSL pattern "filter to
#' qualifying records, order by source preference/date/sequence, then keep the
#' last record" without hardcoding any study values, visits, tests, or terms.
#' By default, ties on the supplied ordering variables are treated as errors so
#' study programs do not silently select an arbitrary source record.
#'
#' @param dataset A data frame or tibble containing candidate records.
#' @param by Character vector of grouping variables. Defaults to `"USUBJID"`.
#' @param order Character vector of variables defining the within-group order.
#'   For `mode = "last"` the maximum ordered record is selected; for
#'   `mode = "first"` the minimum ordered record is selected. Include explicit
#'   sequence/tie-break columns when multiple records can share the same date.
#' @param mode Either `"last"` (default) or `"first"`.
#' @param keep Optional character vector of columns to keep in addition to
#'   `by`. If `NULL`, all columns are returned.
#' @param ties Handling when more than one record in a group has the same
#'   selected `order` values: `"error"` (default), `"first"`, or `"last"`.
#' @param na_last Passed to `base::order()` as `na.last`. Defaults to `TRUE`.
#'
#' @return A data frame containing at most one row per `by` group.
#' @export
select_subject_extreme_record <- function(dataset,
                                          by = "USUBJID",
                                          order,
                                          mode = c("last", "first"),
                                          keep = NULL,
                                          ties = c("error", "first", "last"),
                                          na_last = TRUE) {
  .adamtools_check_data_frame(dataset)
  .adamtools_check_character_vector(by, "by", allow_null = FALSE)
  .adamtools_check_character_vector(order, "order", allow_null = FALSE)
  .adamtools_check_character_vector(keep, "keep", allow_null = TRUE)
  .adamtools_check_columns(dataset, unique(c(by, order, keep)))
  if (any(vapply(dataset[by], function(x) any(is.na(x)), logical(1L)))) {
    stop("`by` grouping variables must not contain missing values.", call. = FALSE)
  }

  mode <- match.arg(mode)
  ties <- match.arg(ties)

  output_cols <- if (is.null(keep)) names(dataset) else unique(c(by, keep))
  if (nrow(dataset) == 0L) {
    return(dataset[FALSE, output_cols, drop = FALSE])
  }

  sort_cols <- unique(c(by, order))
  work <- dataset
  work$.adamtools_orig_row_ <- seq_len(nrow(work))
  ord <- do.call(base::order, c(unname(work[sort_cols]), list(na.last = na_last)))
  work <- work[ord, , drop = FALSE]

  selected <- work %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    { if (identical(mode, "last")) dplyr::slice_tail(., n = 1L) else dplyr::slice_head(., n = 1L) } %>%
    dplyr::ungroup()

  if (identical(ties, "error")) {
    tie_counts <- work %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(unique(c(by, order))))) %>%
      dplyr::summarise(.adamtools_tie_n_ = dplyr::n(), .groups = "drop")

    selected_ties <- selected %>%
      dplyr::left_join(tie_counts, by = unique(c(by, order))) %>%
      dplyr::filter(.adamtools_tie_n_ > 1L)

    if (nrow(selected_ties) > 0L) {
      stop(
        "Non-unique selected record for group ",
        .adamtools_format_key_values(selected_ties[1L, by, drop = FALSE], by, max_rows = 1L),
        ". Add deterministic tie-break columns to `order` or set `ties`.",
        call. = FALSE
      )
    }
  } else {
    selected_order <- selected[, unique(c(by, order)), drop = FALSE]
    selected <- work %>%
      dplyr::inner_join(selected_order, by = unique(c(by, order))) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
      { if (identical(ties, "last")) dplyr::slice_tail(., n = 1L) else dplyr::slice_head(., n = 1L) } %>%
      dplyr::ungroup()
  }

  result <- selected[, output_cols, drop = FALSE]
  row.names(result) <- NULL
  result
}

.adamtools_row_same_values <- function(a, b) {
  if (!identical(names(a), names(b))) {
    return(FALSE)
  }
  values_equal <- Map(function(x, y) {
    x_missing <- is.na(x)
    y_missing <- is.na(y)
    (x_missing & y_missing) | (!x_missing & !y_missing & x == y)
  }, a, b)
  all(unlist(values_equal, use.names = FALSE))
}
