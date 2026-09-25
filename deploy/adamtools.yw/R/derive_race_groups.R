#' Derive race group variables, including a collapsed multi-race category
#'
#' Cleans a raw `RACE` (or similarly coded) variable, detects multi-race
#' responses (values containing a separator such as `","` or `"/"`), and
#' derives a single collapsed `ARACE` value per subject per the requested
#' `multiple_handling` strategy, along with numeric companions and
#' optional recode maps. This generalizes the repeated "standardize RACE for
#' analysis, collapsing free-text multi-race entries" pattern without
#' hardcoding study-specific race value sets.
#'
#' @param dataset A data frame or tibble containing `race_var`.
#' @param race_var Name of the raw race column. Defaults to `"RACE"`.
#' @param separator Regular expression used to detect/split multiple race
#'   values within a single `race_var` value. Defaults to `"[,/]"`.
#' @param multiple_handling How to collapse a multi-race value into
#'   `ARACE`: `"multiple"` (default) recodes any multi-race response to the
#'   literal string `"MULTIPLE"`; `"first"` keeps the first listed race.
#' @param racemap Optional named character vector recoding cleaned `RACE`
#'   values to grouped labels for `RACEGR1` (e.g. `c(WHITE = "White",
#'   "BLACK OR AFRICAN AMERICAN" = "Black or African American")`). Values
#'   not found in `racemap` pass through unchanged. If `NULL` (default),
#'   `RACEGR1` is not derived.
#' @param racenmap Optional named numeric vector giving the `RACEGR1N`
#'   sort-order number for each `RACEGR1` label (names = `RACEGR1` label
#'   values). If `NULL` (default), `RACEGR1N` is derived from the factor
#'   order of the distinct `RACEGR1` values encountered.
#'
#' @return `dataset` with `ARACE`, `ARACEN` and (when `racemap` is
#'   supplied) `RACEGR1`/`RACEGR1N` added.
#' @export
derive_race_groups <- function(dataset,
                               race_var = "RACE",
                               separator = "[,/]",
                               multiple_handling = c("multiple", "first"),
                               racemap = NULL,
                               racenmap = NULL) {
  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }
  if (!race_var %in% names(dataset)) {
    stop("`race_var` is not a column in `dataset`.", call. = FALSE)
  }
  multiple_handling <- match.arg(multiple_handling)

  raw <- dataset[[race_var]]
  cleaned <- trimws(toupper(as.character(raw)))
  is_multi <- grepl(separator, cleaned)

  arace <- cleaned
  if (multiple_handling == "multiple") {
    arace[is_multi] <- "MULTIPLE"
  } else {
    first_val <- vapply(cleaned[is_multi], function(x) {
      trimws(strsplit(x, separator)[[1]][1])
    }, character(1))
    arace[is_multi] <- first_val
  }
  arace[is.na(cleaned)] <- NA_character_

  dataset$ARACE <- arace
  arace_factor <- factor(arace, levels = sort(unique(stats::na.omit(arace))))
  dataset$ARACEN <- as.numeric(arace_factor)

  if (!is.null(racemap)) {
    mapped <- unname(racemap[arace])
    mapped[is.na(mapped) & !is.na(arace)] <- arace[is.na(mapped) & !is.na(arace)]
    dataset$RACEGR1 <- mapped

    if (!is.null(racenmap)) {
      dataset$RACEGR1N <- unname(racenmap[dataset$RACEGR1])
    } else {
      racegr1_factor <- factor(dataset$RACEGR1, levels = sort(unique(stats::na.omit(dataset$RACEGR1))))
      dataset$RACEGR1N <- as.numeric(racegr1_factor)
    }
  }

  dataset
}
