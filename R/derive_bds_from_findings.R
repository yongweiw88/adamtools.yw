#' Derive a core BDS ADaM dataset from an SDTM findings domain
#'
#' High-level orchestration of the repeated "findings domain to BDS" pattern:
#' filters to records with a non-missing result, derives `ADT`/`ADTM`,
#' `PARAMCD`/`PARAM`, `AVAL`/`AVALC`, an analysis day (`ADY`), and optionally
#' baseline/change-from-baseline variables. This function composes existing
#' `admiral` primitives (`admiral::convert_dtc_to_dt()`,
#' `admiral::derive_var_base()`, `admiral::derive_var_chg()`,
#' `admiral::derive_var_pchg()`, and this package's [derive_analysis_day()])
#' rather than reimplementing their logic; it does **not** derive `ABLFL`
#' (baseline-flag assignment is dataset/study-specific and must be done by
#' the caller before invoking this function when `derive_baseline = TRUE`).
#'
#' @param dataset SDTM findings domain data frame (e.g. `VS`, `EG`, `LB`),
#'   with SUPP already merged if needed.
#' @param domain Two-letter SDTM findings domain prefix, e.g. `"VS"`, `"EG"`,
#'   `"LB"`. Used to resolve the source column names (`--TESTCD`, `--TEST`,
#'   `--STRESC`, `--STRESN`, `--DTC`, `--BLFL`).
#' @param param_map Optional data frame with columns `TESTCD`, `PARAMCD`,
#'   `PARAM`, and optionally `PARAMN`, used to map source test codes to
#'   ADaM parameter codes/labels. If `NULL` (default), `PARAMCD`/`PARAM` are
#'   copied directly from `--TESTCD`/`--TEST`.
#' @param ref_date_var Name of the reference date variable (e.g.
#'   `"TRTSDT"`) used to derive `ADY` via [derive_analysis_day()]. If not
#'   present in `dataset`, `ADY` is not derived.
#' @param derive_baseline Logical; derive `BASE`/`BASEC` via
#'   `admiral::derive_var_base()`, using `ABLFL == "Y"` as the baseline
#'   filter. Requires `dataset` to already have `ABLFL` populated. Defaults
#'   to `TRUE`.
#' @param derive_change Logical; derive `CHG`/`PCHG` via
#'   `admiral::derive_var_chg()`/`admiral::derive_var_pchg()`. Only applied
#'   when `derive_baseline = TRUE` (otherwise there is no `BASE` to change
#'   from). Defaults to `TRUE`.
#'
#' @return A BDS-style data frame with `ADT`, `ADTM` (when a time component
#'   is present), `PARAMCD`, `PARAM`, `AVAL`, `AVALC`, `ADY` (when
#'   `ref_date_var` is available), and optionally `BASE`/`BASEC`/`CHG`/
#'   `PCHG`.
#' @export
derive_bds_from_findings <- function(dataset,
                                     domain,
                                     param_map = NULL,
                                     ref_date_var = "TRTSDT",
                                     derive_baseline = TRUE,
                                     derive_change = TRUE) {
  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }
  domain <- toupper(domain)
  testcd_var <- paste0(domain, "TESTCD")
  test_var <- paste0(domain, "TEST")
  stresc_var <- paste0(domain, "STRESC")
  stresn_var <- paste0(domain, "STRESN")
  dtc_var <- paste0(domain, "DTC")
  blfl_var <- paste0(domain, "BLFL")

  required <- c("STUDYID", "USUBJID", testcd_var, test_var, stresc_var, dtc_var)
  missing_vars <- setdiff(required, names(dataset))
  if (length(missing_vars) > 0L) {
    stop(
      "`dataset` is missing required variable(s): ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }

  result <- dataset[!is.na(dataset[[stresc_var]]) & dataset[[stresc_var]] != "", , drop = FALSE]

  dtc_vals <- result[[dtc_var]]
  result$ADT <- admiral::convert_dtc_to_dt(dtc_vals, highest_imputation = "n")
  has_time <- any(grepl("T\\d{2}:\\d{2}", dtc_vals))
  if (has_time) {
    result$ADTM <- admiral::convert_dtc_to_dtm(dtc_vals, highest_imputation = "n")
  }

  if (!is.null(param_map)) {
    required_map <- c("TESTCD", "PARAMCD", "PARAM")
    missing_map <- setdiff(required_map, names(param_map))
    if (length(missing_map) > 0L) {
      stop(
        "`param_map` is missing required column(s): ",
        paste(missing_map, collapse = ", "),
        call. = FALSE
      )
    }
    map_lookup <- param_map[!duplicated(param_map$TESTCD), ]
    idx <- match(result[[testcd_var]], map_lookup$TESTCD)
    result$PARAMCD <- map_lookup$PARAMCD[idx]
    result$PARAM <- map_lookup$PARAM[idx]
    if ("PARAMN" %in% names(map_lookup)) {
      result$PARAMN <- map_lookup$PARAMN[idx]
    }
  } else {
    result$PARAMCD <- result[[testcd_var]]
    result$PARAM <- result[[test_var]]
  }

  result$AVAL <- if (stresn_var %in% names(result)) result[[stresn_var]] else NA_real_
  result$AVALC <- result[[stresc_var]]

  if (ref_date_var %in% names(result)) {
    result <- derive_analysis_day(result, date_var = ADT, anchor_var = !!rlang::sym(ref_date_var), new_var = "ADY")
  }

  if (derive_baseline) {
    if (!blfl_var %in% names(result) && !"ABLFL" %in% names(result)) {
      warning(
        "derive_baseline = TRUE but neither \"", blfl_var, "\" nor \"ABLFL\" is present in `dataset`; ",
        "skipping baseline/change derivation. Derive the baseline flag before calling this function.",
        call. = FALSE
      )
    } else {
      if (!"ABLFL" %in% names(result) && blfl_var %in% names(result)) {
        result$ABLFL <- result[[blfl_var]]
      }
      result <- admiral::derive_var_base(
        result,
        by_vars = admiral::exprs(STUDYID, USUBJID, PARAMCD)
      )
      if (derive_change) {
        result <- admiral::derive_var_chg(result)
        result <- admiral::derive_var_pchg(result)
      }
    }
  }

  result
}
