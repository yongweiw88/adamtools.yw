#' Derive AVISIT/AVISITN from a day/date variable using a visit-window map
#'
#' Table-driven derivation of `AVISIT`, `AVISITN`, and optionally `AWTARGET`/
#' `AWRANGE` from an analysis day or date variable, using a caller-supplied
#' window-map data frame. This generalizes the repeated "map analysis day
#' into study-defined day ranges" pattern (e.g. ADSNAP-style windowing) while
#' keeping the window boundaries themselves entirely study-specific and out
#' of the package. For planned-visit-based `AVISIT`/`AVISITN` (i.e. carrying
#' forward SDTM `VISIT`/`VISITNUM`), do not use this function; simply copy
#' the SDTM visit variables instead.
#'
#' @param dataset A data frame or tibble.
#' @param day_var Name of the day/date variable in `dataset` to map against
#'   the window boundaries (unquoted), e.g. `ADY`.
#' @param window_map A data frame defining the visit windows, with one row
#'   per visit window and (at minimum) the columns named by `lower_var`,
#'   `upper_var`, `avisit_var`, and `avisitn_var`. Boundaries are inclusive
#'   on both ends. May also include `awtarget_var` (target day) and
#'   `weight_var` used to build `AWRANGE` as `"[lower, upper]"` when
#'   `derive_awrange = TRUE`.
#' @param lower_var Column in `window_map` giving the inclusive lower day
#'   bound of each window. Defaults to `"AWLO"`.
#' @param upper_var Column in `window_map` giving the inclusive upper day
#'   bound of each window. Defaults to `"AWHI"`.
#' @param avisit_var Column in `window_map` giving the `AVISIT` value for
#'   each window. Defaults to `"AVISIT"`.
#' @param avisitn_var Column in `window_map` giving the `AVISITN` value for
#'   each window. Defaults to `"AVISITN"`.
#' @param awtarget_var Optional column in `window_map` giving the target day
#'   for each window, copied to a new `AWTARGET` variable on `dataset` when
#'   `derive_awtarget = TRUE`. Defaults to `"AWTARGET"`.
#' @param derive_awtarget Logical; derive `AWTARGET` from `window_map`.
#'   Defaults to `FALSE`.
#' @param derive_awrange Logical; derive an `AWRANGE` character variable
#'   (`"lower to upper"`) from the matched window's bounds. Defaults to
#'   `FALSE`.
#' @param unmatched Behavior when `day_var` does not fall in any window:
#'   `"na"` (default) sets `AVISIT`/`AVISITN` to `NA`, `"warn"` does the same
#'   but issues a warning listing unmatched rows, `"error"` stops with an
#'   error.
#'
#' @return `dataset` with `AVISIT`/`AVISITN` (and optionally `AWTARGET`/
#'   `AWRANGE`) added.
#' @export
derive_analysis_visit_window <- function(dataset,
                                         day_var,
                                         window_map,
                                         lower_var = "AWLO",
                                         upper_var = "AWHI",
                                         avisit_var = "AVISIT",
                                         avisitn_var = "AVISITN",
                                         awtarget_var = "AWTARGET",
                                         derive_awtarget = FALSE,
                                         derive_awrange = FALSE,
                                         unmatched = c("na", "warn", "error")) {
  unmatched <- match.arg(unmatched)
  day_sym <- rlang::ensym(day_var)
  day_name <- rlang::as_name(day_sym)

  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }
  if (!day_name %in% names(dataset)) {
    stop("`day_var` is not a column in `dataset`.", call. = FALSE)
  }
  if (!inherits(window_map, "data.frame")) {
    stop("`window_map` must be a data frame or tibble.", call. = FALSE)
  }

  required_cols <- c(lower_var, upper_var, avisit_var, avisitn_var)
  missing_cols <- setdiff(required_cols, names(window_map))
  if (length(missing_cols) > 0L) {
    stop(
      "`window_map` is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  if (derive_awtarget && !awtarget_var %in% names(window_map)) {
    stop(
      "`derive_awtarget = TRUE` but `awtarget_var` (\"", awtarget_var,
      "\") is not a column in `window_map`.",
      call. = FALSE
    )
  }

  day_values <- dataset[[day_name]]
  n <- length(day_values)

  match_idx <- rep(NA_integer_, n)
  for (i in seq_len(nrow(window_map))) {
    lo <- window_map[[lower_var]][i]
    hi <- window_map[[upper_var]][i]
    in_window <- !is.na(day_values) & day_values >= lo & day_values <= hi
    match_idx[in_window & is.na(match_idx)] <- i
  }

  is_unmatched <- !is.na(day_values) & is.na(match_idx)
  if (any(is_unmatched)) {
    msg <- paste0(
      sum(is_unmatched), " row(s) had a `", day_name,
      "` value that did not fall within any window in `window_map`."
    )
    if (unmatched == "error") {
      stop(msg, call. = FALSE)
    } else if (unmatched == "warn") {
      warning(msg, call. = FALSE)
    }
  }

  dataset[[avisit_var]] <- window_map[[avisit_var]][match_idx]
  dataset[[avisitn_var]] <- window_map[[avisitn_var]][match_idx]

  if (derive_awtarget) {
    dataset[["AWTARGET"]] <- window_map[[awtarget_var]][match_idx]
  }
  if (derive_awrange) {
    lower_vals <- window_map[[lower_var]][match_idx]
    upper_vals <- window_map[[upper_var]][match_idx]
    dataset[["AWRANGE"]] <- ifelse(
      is.na(match_idx),
      NA_character_,
      paste0(lower_vals, " to ", upper_vals)
    )
  }

  dataset
}
