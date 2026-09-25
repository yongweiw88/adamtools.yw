#' Flag the first eligible record within a group
#'
#' Derives an occurrence flag (e.g. `AOCCFL`, `AOCCPFL`, `AOCC01FL`) marking
#' the first record within each `by`-group that satisfies an eligibility
#' condition, after arranging by `order`. This formalizes the repeated
#' `if_else(row_number() == 1 & <condition>, "Y", NA_character_)` pattern
#' used for AE/CM occurrence flags.
#'
#' @param dataset A data frame or tibble.
#' @param flag Name of the new flag variable to create. Defaults to
#'   `"AOCCFL"`.
#' @param by Character vector of grouping variables, e.g.
#'   `c("USUBJID", "AEDECOD")`.
#' @param order Optional list of unquoted expressions (via
#'   `dplyr::arrange()` semantics, e.g. `dplyr::vars(ASTDT, AESEQ)`) or a
#'   character vector of column names giving the sort order within each
#'   group before selecting the first eligible record. If `NULL` (default),
#'   the existing row order is used.
#' @param condition Logical vector (same length as `nrow(dataset)`) or a
#'   single logical value giving the eligibility condition a record must
#'   meet to be flagged. Defaults to `TRUE` (all records eligible).
#' @param true_value Value assigned to `flag` for the first eligible record
#'   in each group. Defaults to `"Y"`.
#' @param false_value Value assigned to `flag` for all other records.
#'   Defaults to `NA_character_`.
#'
#' @return `dataset` with the occurrence flag variable added. Row order is
#'   preserved (the function restores the original row order after any
#'   internal sorting).
#' @export
derive_first_occurrence_flag <- function(dataset,
                                         flag = "AOCCFL",
                                         by,
                                         order = NULL,
                                         condition = TRUE,
                                         true_value = "Y",
                                         false_value = NA_character_) {
  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }
  missing_by <- setdiff(by, names(dataset))
  if (length(missing_by) > 0L) {
    stop(
      "The following `by` variables are not in `dataset`: ",
      paste(missing_by, collapse = ", "),
      call. = FALSE
    )
  }

  result <- dataset
  result$.orig_row_ <- seq_len(nrow(result))
  result$.eligible_ <- condition

  if (!is.null(order)) {
    missing_order <- setdiff(order, names(result))
    if (length(missing_order) > 0L) {
      stop(
        "The following `order` variables are not in `dataset`: ",
        paste(missing_order, collapse = ", "),
        call. = FALSE
      )
    }
    result <- result[do.call(base::order, as.list(result[order])), , drop = FALSE]
  }

  result <- dplyr::group_by(result, dplyr::across(dplyr::all_of(by)))
  result <- dplyr::mutate(
    result,
    .flag_ = {
      idx <- which(.eligible_)
      out <- rep(false_value, dplyr::n())
      if (length(idx) > 0L) out[idx[1]] <- true_value
      out
    }
  )
  result <- dplyr::ungroup(result)

  result <- result[order(result$.orig_row_), , drop = FALSE]
  result[[flag]] <- result$.flag_
  result$.orig_row_ <- NULL
  result$.eligible_ <- NULL
  result$.flag_ <- NULL

  result
}
