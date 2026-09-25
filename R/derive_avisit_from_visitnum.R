#' Derive AVISIT/AVISITN by mapping SDTM VISITNUM directly (no day windowing)
#'
#' Table-driven derivation of `AVISIT`/`AVISITN` for domains with a sparse,
#' planned-visit-only schedule (e.g. Vital Signs), where `AVISITN` is simply
#' the scheduled `VISITNUM` and unscheduled (decimal-suffixed) `VISITNUM`
#' values collapse to a single "unscheduled" analysis visit. This is the
#' direct-mapping counterpart to [derive_analysis_visit_window()], which maps
#' a day/date variable into study-defined day ranges instead; use that
#' function when the domain needs day-based windowing, and this one when the
#' SDTM visit structure itself is the analysis visit structure.
#'
#' @param dataset A data frame or tibble.
#' @param visitnum_var Name of the numeric SDTM visit-number variable
#'   (unquoted), e.g. `VISITNUM`. Non-integer (decimal-suffixed) values are
#'   treated as unscheduled/unplanned visits.
#' @param label_map Optional data frame with columns `VISITNUM` and `AVISIT`
#'   giving the analysis-visit text label for each scheduled `VISITNUM`. If
#'   `NULL` (default), or a scheduled `VISITNUM` has no row in `label_map`,
#'   `AVISIT` falls back to `paste0("VISITNUM ", VISITNUM)`.
#' @param unscheduled_avisitn Numeric `AVISITN` value assigned to
#'   unscheduled (decimal-suffixed) `VISITNUM` records. Defaults to `900`.
#' @param unscheduled_avisit Character `AVISIT` value assigned to
#'   unscheduled records. Defaults to `"UNSCHEDULED"`.
#' @param avisit_var Name of the new `AVISIT` column to create. Defaults to
#'   `"AVISIT"`.
#' @param avisitn_var Name of the new `AVISITN` column to create. Defaults to
#'   `"AVISITN"`.
#'
#' @return `dataset` with `avisit_var`/`avisitn_var` added.
#' @export
derive_avisit_from_visitnum <- function(dataset,
                                        visitnum_var,
                                        label_map = NULL,
                                        unscheduled_avisitn = 900,
                                        unscheduled_avisit = "UNSCHEDULED",
                                        avisit_var = "AVISIT",
                                        avisitn_var = "AVISITN") {
  visitnum_sym <- rlang::ensym(visitnum_var)
  visitnum_name <- rlang::as_name(visitnum_sym)

  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }
  if (!visitnum_name %in% names(dataset)) {
    stop("`visitnum_var` is not a column in `dataset`.", call. = FALSE)
  }
  if (!is.null(label_map)) {
    missing_cols <- setdiff(c("VISITNUM", "AVISIT"), names(label_map))
    if (length(missing_cols) > 0L) {
      stop("`label_map` is missing required column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
  }

  visitnum <- dataset[[visitnum_name]]
  is_scheduled <- !is.na(visitnum) & visitnum == round(visitnum)

  avisitn <- ifelse(is_scheduled, visitnum, unscheduled_avisitn)

  if (is.null(label_map)) {
    matched_label <- rep(NA_character_, length(visitnum))
  } else {
    matched_label <- unname(stats::setNames(label_map$AVISIT, label_map$VISITNUM)[as.character(visitnum)])
  }

  avisit <- ifelse(
    is_scheduled & !is.na(matched_label),
    matched_label,
    ifelse(is_scheduled, paste0("VISITNUM ", visitnum), unscheduled_avisit)
  )
  avisit[is.na(visitnum)] <- NA_character_
  avisitn[is.na(visitnum)] <- NA_real_

  dataset[[avisit_var]] <- avisit
  dataset[[avisitn_var]] <- avisitn
  dataset
}
