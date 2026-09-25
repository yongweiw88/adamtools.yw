#' Derive exposure timing context for ISR AE records
#'
#' Adds caller-named exposure context variables to an ISR-style AE analysis data
#' set. For each AE row, the function searches caller-provided injection,
#' infusion, or other exposure records using subject/key variables and optional
#' AE-to-exposure category matching. It derives the first qualifying exposure
#' date on or before the AE date, the latest qualifying exposure date on or
#' before the AE date, an on-or-after-first-exposure flag, and optional
#' variables copied from the selected latest exposure row (for example a visit
#' label or visit number).
#'
#' This helper is intentionally study-agnostic. It does not filter exposure
#' routes, dose forms, treatments, visits, windows, or AE terms. Callers must
#' prepare the AE and exposure input records and any category mapping before
#' calling this function.
#'
#' The return grain is exactly one row per input AE row, in the original order.
#' Exposures after the AE date are never selected for first/latest context. AE
#' rows with missing AE dates, no matching category, or no prior/current
#' exposure receive missing context dates and the configured "no" flag.
#'
#' @param ae_dataset AE analysis data frame; one output row is returned for
#'   each input row.
#' @param exposure_dataset Exposure data frame containing one or more
#'   qualifying exposure records prepared by the caller.
#' @param by_vars Character vector of key columns present in both data frames,
#'   typically subject and any treatment/period keys needed to make exposure
#'   matching unambiguous.
#' @param ae_date_var Name of the AE analysis date/datetime column.
#' @param exposure_date_var Name of the exposure date/datetime column.
#' @param ae_category_var Optional AE category column used for category
#'   matching.
#' @param exposure_category_var Optional exposure category column used for
#'   category matching.
#' @param category_map Optional data frame mapping AE category values to
#'   exposure category values. When supplied, `ae_category_var` and
#'   `exposure_category_var` are required. If omitted and both category
#'   variables are supplied, categories are matched by equality. If all three
#'   are omitted, exposure matching uses only `by_vars`.
#' @param map_ae_category_var Column in `category_map` containing AE category
#'   values. Defaults to `"ae_category"`.
#' @param map_exposure_category_var Column in `category_map` containing
#'   exposure category values. Defaults to `"exposure_category"`.
#' @param first_exposure_date_var Output column for the first qualifying
#'   exposure date on or before the AE date. Defaults to `"FIRSTEXDT"`.
#' @param latest_exposure_date_var Output column for the latest qualifying
#'   exposure date on or before the AE date. Defaults to `"LASTEXDT"`.
#' @param on_or_after_first_flag_var Output column for the on/after-first
#'   exposure flag. Defaults to `"ANL01FL"`.
#' @param latest_exposure_vars Optional named character vector mapping output
#'   column names to columns copied from the selected latest exposure row, for
#'   example `c(LSTVISIT = "VISIT", LSTVISITN = "VISITNUM")`.
#' @param flag_values Character vector giving yes/no flag values. Use names
#'   `yes` and `no` or provide exactly two values in yes, no order. Defaults to
#'   `c(yes = "Y", no = "N")`.
#' @param duplicate_exposure_action How to handle exposure records duplicated
#'   within `by_vars`, exposure category, and exposure date. `"error"` stops;
#'   `"distinct"` first removes deterministic duplicates across the matching
#'   keys, exposure date, and requested copied exposure variables, then errors
#'   if ambiguity remains. Defaults to `"error"`.
#' @param unmatched_category Policy for AE categories not present in
#'   `category_map`. `"error"` stops; `"ignore"` leaves those AE rows with
#'   missing context and the no flag. Defaults to `"error"`.
#' @param overwrite Logical; allow output columns to replace existing
#'   `ae_dataset` columns. Defaults to `FALSE`.
#'
#' @return `ae_dataset` with requested exposure context columns added.
#' @export
derive_isr_exposure_context <- function(ae_dataset,
                                        exposure_dataset,
                                        by_vars,
                                        ae_date_var,
                                        exposure_date_var,
                                        ae_category_var = NULL,
                                        exposure_category_var = NULL,
                                        category_map = NULL,
                                        map_ae_category_var = "ae_category",
                                        map_exposure_category_var = "exposure_category",
                                        first_exposure_date_var = "FIRSTEXDT",
                                        latest_exposure_date_var = "LASTEXDT",
                                        on_or_after_first_flag_var = "ANL01FL",
                                        latest_exposure_vars = NULL,
                                        flag_values = c(yes = "Y", no = "N"),
                                        duplicate_exposure_action = c("error", "distinct"),
                                        unmatched_category = c("error", "ignore"),
                                        overwrite = FALSE) {
  .adamtools_check_data_frame(ae_dataset, "ae_dataset")
  .adamtools_check_data_frame(exposure_dataset, "exposure_dataset")
  .adamtools_check_character_vector(by_vars, "by_vars", allow_null = FALSE)
  if (any(duplicated(by_vars))) {
    stop("`by_vars` must not contain duplicate column names.", call. = FALSE)
  }
  .adamtools_check_scalar_name(ae_date_var, "ae_date_var")
  .adamtools_check_scalar_name(exposure_date_var, "exposure_date_var")
  .adamtools_check_scalar_name(ae_category_var, "ae_category_var", allow_null = TRUE)
  .adamtools_check_scalar_name(exposure_category_var, "exposure_category_var", allow_null = TRUE)
  .adamtools_check_scalar_name(first_exposure_date_var, "first_exposure_date_var")
  .adamtools_check_scalar_name(latest_exposure_date_var, "latest_exposure_date_var")
  .adamtools_check_scalar_name(on_or_after_first_flag_var, "on_or_after_first_flag_var")
  .adamtools_check_columns(ae_dataset, c(by_vars, ae_date_var, ae_category_var), "ae_dataset")
  .adamtools_check_columns(exposure_dataset, c(by_vars, exposure_date_var, exposure_category_var), "exposure_dataset")

  duplicate_exposure_action <- match.arg(duplicate_exposure_action)
  unmatched_category <- match.arg(unmatched_category)
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("`overwrite` must be a single TRUE/FALSE value.", call. = FALSE)
  }

  if (!is.null(latest_exposure_vars)) {
    if (!is.character(latest_exposure_vars) ||
        length(latest_exposure_vars) == 0L ||
        is.null(names(latest_exposure_vars)) ||
        any(names(latest_exposure_vars) == "") ||
        any(is.na(latest_exposure_vars)) ||
        any(latest_exposure_vars == "")) {
      stop(
        "`latest_exposure_vars` must be a named character vector mapping output names to exposure columns.",
        call. = FALSE
      )
    }
    .adamtools_check_columns(exposure_dataset, unname(latest_exposure_vars), "exposure_dataset")
  }
  context_internal_vars <- if (is.null(latest_exposure_vars)) {
    character()
  } else {
    paste0(".adamtools_ctx_", seq_along(latest_exposure_vars))
  }

  flag_values <- .adamtools_normalize_flag_values(flag_values)
  output_columns <- c(
    first_exposure_date_var,
    latest_exposure_date_var,
    on_or_after_first_flag_var,
    names(latest_exposure_vars)
  )
  .adamtools_check_output_columns(ae_dataset, output_columns, overwrite = overwrite)

  internal_cols <- c(
    ".adamtools_orig_row_",
    ".adamtools_match_category_",
    ".adamtools_map_ae_category_",
    ".adamtools_ae_date_",
    ".adamtools_ex_date_",
    context_internal_vars
  )
  .adamtools_check_internal_columns(ae_dataset, internal_cols)
  .adamtools_check_internal_columns(exposure_dataset, internal_cols)

  .adamtools_check_date_like(ae_dataset[[ae_date_var]], "ae_date_var")
  .adamtools_check_date_like(exposure_dataset[[exposure_date_var]], "exposure_date_var")

  category_mode <- .adamtools_isr_category_mode(ae_category_var, exposure_category_var, category_map)
  ae_expanded <- ae_dataset
  ae_expanded$.adamtools_orig_row_ <- seq_len(nrow(ae_expanded))
  ae_expanded$.adamtools_ae_date_ <- ae_expanded[[ae_date_var]]

  exposure_prepped <- exposure_dataset
  exposure_prepped$.adamtools_ex_date_ <- exposure_prepped[[exposure_date_var]]

  if (identical(category_mode, "none")) {
    ae_expanded$.adamtools_match_category_ <- "__ADAMTOOLS_ALL_EXPOSURES__"
    exposure_prepped$.adamtools_match_category_ <- "__ADAMTOOLS_ALL_EXPOSURES__"
  } else if (identical(category_mode, "direct")) {
    ae_expanded$.adamtools_match_category_ <- as.character(ae_expanded[[ae_category_var]])
    exposure_prepped$.adamtools_match_category_ <- as.character(exposure_prepped[[exposure_category_var]])
  } else {
    .adamtools_check_data_frame(category_map, "category_map")
    .adamtools_check_scalar_name(map_ae_category_var, "map_ae_category_var")
    .adamtools_check_scalar_name(map_exposure_category_var, "map_exposure_category_var")
    .adamtools_check_columns(category_map, c(map_ae_category_var, map_exposure_category_var), "category_map")
    if (any(is.na(category_map[[map_ae_category_var]])) ||
        any(is.na(category_map[[map_exposure_category_var]]))) {
      stop("`category_map` must not contain missing category values.", call. = FALSE)
    }
    duplicate_map <- duplicated(category_map[c(map_ae_category_var, map_exposure_category_var)]) |
      duplicated(category_map[c(map_ae_category_var, map_exposure_category_var)], fromLast = TRUE)
    if (any(duplicate_map)) {
      examples <- .adamtools_format_key_values(
        category_map[duplicate_map, , drop = FALSE],
        c(map_ae_category_var, map_exposure_category_var)
      )
      stop("`category_map` contains duplicate mapping rows: ", examples, call. = FALSE)
    }

    map_for_join <- unique(data.frame(
      .adamtools_map_ae_category_ = as.character(category_map[[map_ae_category_var]]),
      .adamtools_match_category_ = as.character(category_map[[map_exposure_category_var]]),
      stringsAsFactors = FALSE
    ))
    ae_expanded$.adamtools_map_ae_category_ <- as.character(ae_expanded[[ae_category_var]])
    ae_expanded <- merge(
      ae_expanded,
      map_for_join,
      by = ".adamtools_map_ae_category_",
      all.x = TRUE,
      sort = FALSE
    )
    ae_expanded$.adamtools_map_ae_category_ <- NULL

    unmatched <- is.na(ae_expanded$.adamtools_match_category_)
    if (any(unmatched) && identical(unmatched_category, "error")) {
      examples <- .adamtools_format_key_values(
        ae_expanded[unmatched, , drop = FALSE],
        c(ae_category_var)
      )
      stop("AE category value(s) are not present in `category_map`: ", examples, call. = FALSE)
    }
    exposure_prepped$.adamtools_match_category_ <- as.character(exposure_prepped[[exposure_category_var]])
  }

  if (!identical(category_mode, "none")) {
    ae_expanded <- ae_expanded[!is.na(ae_expanded$.adamtools_match_category_), , drop = FALSE]
    exposure_prepped <- exposure_prepped[!is.na(exposure_prepped$.adamtools_match_category_), , drop = FALSE]
  }

  for (i in seq_along(latest_exposure_vars)) {
    exposure_prepped[[paste0(".adamtools_ctx_", i)]] <- exposure_prepped[[latest_exposure_vars[[i]]]]
  }

  exposure_prepped <- .adamtools_resolve_exposure_duplicates(
    exposure_prepped = exposure_prepped,
    by_vars = by_vars,
    context_internal_vars = context_internal_vars,
    duplicate_exposure_action = duplicate_exposure_action
  )

  exposure_keep <- c(
    by_vars,
    ".adamtools_match_category_",
    ".adamtools_ex_date_",
    context_internal_vars
  )
  exposure_match <- exposure_prepped[, exposure_keep, drop = FALSE]
  ae_match <- ae_expanded[, c(".adamtools_orig_row_", by_vars, ".adamtools_match_category_", ".adamtools_ae_date_"), drop = FALSE]

  candidates <- merge(
    ae_match,
    exposure_match,
    by = c(by_vars, ".adamtools_match_category_"),
    all = FALSE,
    sort = FALSE
  )
  candidates <- candidates[
    !is.na(candidates$.adamtools_ae_date_) &
      !is.na(candidates$.adamtools_ex_date_) &
      candidates$.adamtools_ex_date_ <= candidates$.adamtools_ae_date_,
    ,
    drop = FALSE
  ]

  first_values <- .adamtools_missing_like(exposure_dataset[[exposure_date_var]], nrow(ae_dataset))
  latest_values <- .adamtools_missing_like(exposure_dataset[[exposure_date_var]], nrow(ae_dataset))
  context_values <- stats::setNames(
    vector("list", length(latest_exposure_vars)),
    names(latest_exposure_vars)
  )
  for (out_name in names(context_values)) {
    context_values[[out_name]] <- rep(NA, nrow(ae_dataset))
  }

  if (nrow(candidates) > 0L) {
    first_by_row <- stats::aggregate(
      candidates[".adamtools_ex_date_"],
      by = candidates[".adamtools_orig_row_"],
      FUN = min
    )
    first_values[first_by_row$.adamtools_orig_row_] <- first_by_row$.adamtools_ex_date_

    latest_by_row <- stats::aggregate(
      candidates[".adamtools_ex_date_"],
      by = candidates[".adamtools_orig_row_"],
      FUN = max
    )
    names(latest_by_row)[names(latest_by_row) == ".adamtools_ex_date_"] <- ".adamtools_latest_date_"
    latest_candidates <- dplyr::inner_join(candidates, latest_by_row, by = ".adamtools_orig_row_")
    latest_candidates <- latest_candidates[
      latest_candidates$.adamtools_ex_date_ == latest_candidates$.adamtools_latest_date_,
      ,
      drop = FALSE
    ]
    ambiguous <- duplicated(latest_candidates$.adamtools_orig_row_) |
      duplicated(latest_candidates$.adamtools_orig_row_, fromLast = TRUE)
    if (any(ambiguous)) {
      examples <- .adamtools_format_key_values(
        latest_candidates[ambiguous, , drop = FALSE],
        c(".adamtools_orig_row_", by_vars, ".adamtools_match_category_", ".adamtools_ex_date_")
      )
      stop(
        "Latest exposure selection is ambiguous for AE row(s): ",
        examples,
        ". Add matching keys/categories or make exposure records distinct.",
        call. = FALSE
      )
    }

    latest_values[latest_candidates$.adamtools_orig_row_] <- latest_candidates$.adamtools_ex_date_
    for (i in seq_along(latest_exposure_vars)) {
      out_name <- names(latest_exposure_vars)[i]
      ctx_name <- paste0(".adamtools_ctx_", i)
      context_values[[out_name]][latest_candidates$.adamtools_orig_row_] <- latest_candidates[[ctx_name]]
    }
  }

  flag <- rep(flag_values[["no"]], nrow(ae_dataset))
  flag[!is.na(first_values)] <- flag_values[["yes"]]

  result <- ae_dataset
  result[[first_exposure_date_var]] <- first_values
  result[[latest_exposure_date_var]] <- latest_values
  result[[on_or_after_first_flag_var]] <- flag
  for (out_name in names(context_values)) {
    result[[out_name]] <- context_values[[out_name]]
  }

  result
}

.adamtools_isr_category_mode <- function(ae_category_var, exposure_category_var, category_map) {
  if (is.null(category_map)) {
    if (is.null(ae_category_var) && is.null(exposure_category_var)) {
      return("none")
    }
    if (!is.null(ae_category_var) && !is.null(exposure_category_var)) {
      return("direct")
    }
    stop(
      "`ae_category_var` and `exposure_category_var` must be supplied together, ",
      "or both omitted.",
      call. = FALSE
    )
  }
  if (is.null(ae_category_var) || is.null(exposure_category_var)) {
    stop(
      "`ae_category_var` and `exposure_category_var` are required when `category_map` is supplied.",
      call. = FALSE
    )
  }
  "map"
}

.adamtools_normalize_flag_values <- function(flag_values) {
  if (!is.character(flag_values) || length(flag_values) != 2L || any(is.na(flag_values))) {
    stop("`flag_values` must contain exactly two non-missing character values.", call. = FALSE)
  }
  if (all(c("yes", "no") %in% names(flag_values))) {
    return(flag_values[c("yes", "no")])
  }
  stats::setNames(flag_values, c("yes", "no"))
}

.adamtools_check_date_like <- function(x, arg) {
  if (!(inherits(x, "Date") || inherits(x, "POSIXt") || is.numeric(x))) {
    stop("`", arg, "` must identify a Date, datetime, or numeric analysis-day column.", call. = FALSE)
  }
  invisible(TRUE)
}

.adamtools_missing_like <- function(x, n) {
  if (inherits(x, "Date")) {
    return(as.Date(rep(NA_character_, n)))
  }
  if (inherits(x, "POSIXct")) {
    tz <- attr(x, "tzone")
    if (is.null(tz) || length(tz) == 0L) {
      tz <- "UTC"
    }
    return(as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = tz[[1L]]))
  }
  rep(NA_real_, n)
}

.adamtools_resolve_exposure_duplicates <- function(exposure_prepped,
                                                   by_vars,
                                                   context_internal_vars,
                                                   duplicate_exposure_action) {
  duplicate_key_cols <- c(by_vars, ".adamtools_match_category_", ".adamtools_ex_date_")
  check_data <- exposure_prepped[!is.na(exposure_prepped$.adamtools_ex_date_), , drop = FALSE]

  if (identical(duplicate_exposure_action, "distinct")) {
    distinct_cols <- c(duplicate_key_cols, context_internal_vars)
    keep <- !duplicated(check_data[distinct_cols])
    check_data <- check_data[keep, , drop = FALSE]
    missing_date_data <- exposure_prepped[is.na(exposure_prepped$.adamtools_ex_date_), , drop = FALSE]
    exposure_prepped <- dplyr::bind_rows(missing_date_data, check_data)
  }

  check_data <- exposure_prepped[!is.na(exposure_prepped$.adamtools_ex_date_), , drop = FALSE]
  duplicate_rows <- duplicated(check_data[duplicate_key_cols]) |
    duplicated(check_data[duplicate_key_cols], fromLast = TRUE)
  if (any(duplicate_rows)) {
    examples <- .adamtools_format_key_values(
      check_data[duplicate_rows, , drop = FALSE],
      duplicate_key_cols
    )
    stop(
      "`exposure_dataset` contains duplicate exposure records within matching keys/category/date: ",
      examples,
      call. = FALSE
    )
  }

  exposure_prepped
}
