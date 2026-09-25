#' Derive log-transformed value, baseline, and change variables
#'
#' Derives log-transformed counterparts of `AVAL`/`BASE`/`CHG`-style
#' variables (e.g. `LOGVAL`/`LOGBASE`/`LOGCHG` for natural log, or
#' `L10AVAL`/`L10BASE`/`L10CHG` for log base 10), a repeated pattern across
#' biomarker, lipid, and virology ADaM datasets. Any qualitative-value
#' imputation (e.g. handling values below a limit of quantification) must be
#' applied by the caller before calling this function; it is intentionally
#' left out to keep this helper study/domain agnostic.
#'
#' @param dataset A data frame or tibble.
#' @param aval Name of the analysis value variable (unquoted). Defaults to
#'   `AVAL`.
#' @param base Name of the baseline value variable (unquoted). Defaults to
#'   `BASE`. Set to `NULL` to skip deriving the log-baseline/log-change
#'   variables and only derive the log value.
#' @param log_aval Name of the new log-value variable to create. Defaults to
#'   `"LOGVAL"`.
#' @param log_base Name of the new log-baseline variable to create.
#'   Defaults to `"LOGBASE"`.
#' @param log_chg Name of the new log-change variable to create. Defaults to
#'   `"LOGCHG"`.
#' @param base_fn Log function to apply. Defaults to `log` (natural log);
#'   pass `log10` for base-10 transforms (pairing well with, e.g.,
#'   `log_aval = "L10AVAL"`).
#' @param require_positive Logical; if `TRUE` (default), values `<= 0` are
#'   set to `NA` before transforming (log of non-positive numbers is
#'   undefined) rather than raising an error/warning from `base_fn`.
#'
#' @return `dataset` with the log-value variable added, and (when `base` is
#'   not `NULL` and present) the log-baseline and log-change variables
#'   added.
#' @export
derive_log_change_vars <- function(dataset,
                                   aval = AVAL,
                                   base = BASE,
                                   log_aval = "LOGVAL",
                                   log_base = "LOGBASE",
                                   log_chg = "LOGCHG",
                                   base_fn = log,
                                   require_positive = TRUE) {
  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }

  aval_sym <- rlang::ensym(aval)
  aval_name <- rlang::as_name(aval_sym)
  if (!aval_name %in% names(dataset)) {
    stop("`aval` is not a column in `dataset`.", call. = FALSE)
  }

  safe_log <- function(x) {
    if (require_positive) {
      x <- ifelse(!is.na(x) & x <= 0, NA_real_, x)
    }
    base_fn(x)
  }

  result <- dataset
  result[[log_aval]] <- safe_log(result[[aval_name]])

  base_quo <- rlang::enquo(base)
  if (!rlang::quo_is_null(base_quo)) {
    base_name <- rlang::as_name(base_quo)
    if (base_name %in% names(result)) {
      result[[log_base]] <- safe_log(result[[base_name]])
      result[[log_chg]] <- result[[log_aval]] - result[[log_base]]
    }
  }

  result
}
