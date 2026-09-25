#' Derive age group variables from a numeric age
#'
#' Derives a categorical age-group variable (`AGEGR1`) and its sort-order
#' numeric companion (`AGEGR1N`) from a numeric `AGE` column, using
#' caller-supplied cutpoints rather than hardcoded study cutoffs. Cutpoints
#' are treated as left-closed/right-open intervals (i.e. `cut(..., right =
#' FALSE)`), matching the common ADaM convention (e.g. cutpoints
#' `c(18, 65)` yield groups `"<18"`, `"18-64"`, `">=65"`).
#'
#' @param dataset A data frame or tibble containing `age_var`.
#' @param age_var Name of the numeric age column. Defaults to `"AGE"`.
#' @param cutpoints Numeric vector of interior cutpoints (do not include
#'   `-Inf`/`Inf`), e.g. `c(18, 65)` for groups `<18`, `18-64`, `>=65`.
#' @param labels Optional character vector of labels, one longer than
#'   `cutpoints` (one per resulting group), in increasing order. If `NULL`
#'   (default), labels are auto-generated as `"<c1"`, `"c1-c2"`, ...,
#'   `">=cn"`.
#' @param agegr1_var Name of the new character age-group variable. Defaults
#'   to `"AGEGR1"`.
#' @param agegr1n_var Name of the new numeric age-group-order variable.
#'   Defaults to `"AGEGR1N"`.
#'
#' @return `dataset` with `agegr1_var` and `agegr1n_var` added.
#' @export
derive_age_groups <- function(dataset,
                              age_var = "AGE",
                              cutpoints,
                              labels = NULL,
                              agegr1_var = "AGEGR1",
                              agegr1n_var = "AGEGR1N") {
  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }
  if (!age_var %in% names(dataset)) {
    stop("`age_var` is not a column in `dataset`.", call. = FALSE)
  }
  if (missing(cutpoints) || length(cutpoints) == 0L) {
    stop("`cutpoints` must be a non-empty numeric vector of interior breakpoints.", call. = FALSE)
  }
  cutpoints <- sort(cutpoints)

  if (is.null(labels)) {
    labels <- character(length(cutpoints) + 1L)
    labels[1] <- paste0("<", cutpoints[1])
    if (length(cutpoints) > 1L) {
      for (i in seq_len(length(cutpoints) - 1L)) {
        labels[i + 1] <- paste0(cutpoints[i], "-", cutpoints[i + 1] - 1)
      }
    }
    labels[length(labels)] <- paste0(">=", cutpoints[length(cutpoints)])
  } else if (length(labels) != length(cutpoints) + 1L) {
    stop("`labels` must have length one more than `cutpoints`.", call. = FALSE)
  }

  breaks <- c(-Inf, cutpoints, Inf)
  age_vals <- dataset[[age_var]]
  grp <- cut(age_vals, breaks = breaks, labels = labels, right = FALSE)

  dataset[[agegr1_var]] <- as.character(grp)
  dataset[[agegr1n_var]] <- as.numeric(grp)
  dataset
}
