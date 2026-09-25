#' Derive a DS-based disposition STAT/WRES/WSRES parameter triplet
#'
#' Many ADDS-style disposition datasets repeat the same shape for each
#' disposition "family" (e.g. screening, treatment discontinuation, study
#' conclusion, phase conclusion): a `*STAT` completion-status record for
#' every row in the family, a `*WRES` withdrawal-reason record for the
#' subset that didn't complete, and a `*WSRES` withdrawal-sub-reason
#' record for the further subset of those with sub-reason detail
#' (`term1_var` or a colon-delimited `term_var`). This formalizes that
#' repeated three-PARAMCD shape, found written out nearly identically four
#' times (screening/treatment/study/phase families) in one ADDS build.
#'
#' @param dataset A DS-like data frame already filtered to the relevant
#'   `DSSCAT`/family (e.g. `ds |> filter(DSSCAT == "STUDY CONCLUSION")`).
#'   Must contain `STUDYID`, `USUBJID`, `DSSEQ`, `DSTERM`, `decod_var`,
#'   `DSCAT`, `DSSCAT`, `DSSTDTC`, and `term1_var`.
#' @param stat_paramcd,stat_param,stat_paramn PARAMCD/PARAM/PARAMN for the
#'   completion-status record, produced for every row in `dataset`.
#' @param wres_paramcd,wres_param,wres_paramn PARAMCD/PARAM/PARAMN for the
#'   withdrawal-reason record, produced only for rows where `decod_var !=
#'   completed_value`.
#' @param wsres_paramcd,wsres_param,wsres_paramn PARAMCD/PARAM/PARAMN for
#'   the withdrawal-sub-reason record, produced only for the `wres` subset
#'   where `term1_var` is non-missing or `term_var` contains a colon.
#' @param parcat1,parcat1n Category 1 (text/numeric) applied to all three
#'   parameter records.
#' @param stat_avalc_fn Function taking the `decod_var` vector and
#'   returning the `*STAT` AVALC vector. Defaults to
#'   `COMPLETED`/`DISCONTINUED`; pass a custom function for families with
#'   different status labels (e.g. `function(x) if_else(x == "COMPLETED",
#'   "COMPLETED", "WITHDRAWN")` for a study-conclusion family, or a
#'   FAILED/ENTERED-INTO-TRIAL mapping for a screening family).
#' @param completed_value Value of `decod_var` meaning "completed" (this
#'   subject/record does NOT get a `*WRES`/`*WSRES` record). Defaults to
#'   `"COMPLETED"`.
#' @param decod_var,term_var,term1_var Column names (as strings) for the
#'   decoded reason, verbatim term, and first supplemental sub-reason term.
#'   Default to the SDTM DS convention `"DSDECOD"`/`"DSTERM"`/`"DSTERM1"`.
#' @param srcdom Value for `SRCDOM` on every produced record. Defaults to
#'   `"DS"`.
#'
#' @return A data frame (via `dplyr::bind_rows()`) with one row per
#'   produced parameter record, columns `STUDYID`, `USUBJID`, `DSSEQ`,
#'   `DSTERM`, `<decod_var>`, `DSCAT`, `DSSCAT`, `DSSTDTC`, `PARAMCD`,
#'   `PARAM`, `PARAMN`, `PARCAT1`, `PARCAT1N`, `AVALC`, `SRCDOM`, `SRCVAR`,
#'   `SRCSEQ`. Bind this together with the other families' output and any
#'   other parameters (e.g. AE-derived) to build the full ADDS.
#'
#' @export
derive_disposition_triplet <- function(dataset,
                                       stat_paramcd, stat_param, stat_paramn,
                                       wres_paramcd, wres_param, wres_paramn,
                                       wsres_paramcd, wsres_param, wsres_paramn,
                                       parcat1, parcat1n,
                                       stat_avalc_fn = function(decod) {
                                         dplyr::if_else(decod == "COMPLETED", "COMPLETED", "DISCONTINUED")
                                       },
                                       completed_value = "COMPLETED",
                                       decod_var = "DSDECOD",
                                       term_var = "DSTERM",
                                       term1_var = "DSTERM1",
                                       srcdom = "DS") {
  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }
  required <- c("STUDYID", "USUBJID", "DSSEQ", term_var, decod_var, "DSCAT", "DSSCAT", "DSSTDTC", term1_var)
  missing_vars <- setdiff(required, names(dataset))
  if (length(missing_vars) > 0L) {
    stop("`dataset` is missing required column(s): ", paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  decod <- dataset[[decod_var]]
  term <- dataset[[term_var]]
  term1 <- dataset[[term1_var]]

  base_cols <- dataset[, c("STUDYID", "USUBJID", "DSSEQ", term_var, decod_var, "DSCAT", "DSSCAT", "DSSTDTC")]

  stat <- base_cols
  stat$PARAMCD <- stat_paramcd
  stat$PARAM <- stat_param
  stat$PARAMN <- stat_paramn
  stat$PARCAT1 <- parcat1
  stat$PARCAT1N <- parcat1n
  stat$AVALC <- stat_avalc_fn(decod)
  stat$SRCDOM <- srcdom
  stat$SRCVAR <- decod_var
  stat$SRCSEQ <- dataset$DSSEQ

  wres_idx <- !is.na(decod) & decod != completed_value
  wres <- base_cols[wres_idx, , drop = FALSE]
  wres$PARAMCD <- wres_paramcd
  wres$PARAM <- wres_param
  wres$PARAMN <- wres_paramn
  wres$PARCAT1 <- parcat1
  wres$PARCAT1N <- parcat1n
  wres$AVALC <- decod[wres_idx]
  wres$SRCDOM <- srcdom
  wres$SRCVAR <- decod_var
  wres$SRCSEQ <- dataset$DSSEQ[wres_idx]

  has_subreason <- wres_idx & (!is.na(term1) | grepl(":", ifelse(is.na(term), "", term), fixed = TRUE))
  wsres <- base_cols[has_subreason, , drop = FALSE]
  term1_sub <- term1[has_subreason]
  term_sub <- ifelse(is.na(term[has_subreason]), "", term[has_subreason])
  split_from_term <- if (length(term_sub) == 0L) {
    character(0)
  } else {
    trimws(vapply(strsplit(term_sub, ":", fixed = TRUE),
                   function(x) if (length(x) >= 2) x[[2]] else NA_character_,
                   character(1)))
  }
  wsres$PARAMCD <- wsres_paramcd
  wsres$PARAM <- wsres_param
  wsres$PARAMN <- wsres_paramn
  wsres$PARCAT1 <- parcat1
  wsres$PARCAT1N <- parcat1n
  wsres$AVALC <- if (length(term1_sub) == 0L) character(0) else ifelse(!is.na(term1_sub), term1_sub, split_from_term)
  wsres$SRCDOM <- srcdom
  wsres$SRCVAR <- if (length(term1_sub) == 0L) character(0) else ifelse(!is.na(term1_sub), term1_var, term_var)
  wsres$SRCSEQ <- dataset$DSSEQ[has_subreason]

  dplyr::bind_rows(stat, wres, wsres)
}
