#' Format a duration in minutes as a "Xd Xh Xm" character string
#'
#' Formats a numeric duration (in minutes) as a `"<days>d <hours>h
#' <minutes>m"` string, the common ADaM convention for character duration
#' variables such as `ADURC`, `APFTRSTC` (time from first dose to event),
#' and `ALTRTSTC` (time from last dose to event). This formalizes the
#' repeated `paste0(x %/% 1440, "d ", ...)` -style formatting seen across
#' AE/CM/EX-derived duration variables.
#'
#' @param minutes Numeric vector of durations in minutes. May be negative
#'   (e.g. a "time before" duration); the sign is preserved on the days
#'   component only, matching integer-division truncation-toward-zero for
#'   negative inputs is not attempted -- callers needing a signed
#'   before/after duration should take `abs()` first and prepend the sign
#'   themselves.
#'
#' @return A character vector, `NA_character_` where `minutes` is `NA`.
#' @export
format_duration_dhm <- function(minutes) {
  if (!is.numeric(minutes)) {
    stop("`minutes` must be numeric.", call. = FALSE)
  }
  ifelse(
    is.na(minutes), NA_character_,
    paste0(minutes %/% 1440, "d ", (minutes %% 1440) %/% 60, "h ", round(minutes %% 60), "m")
  )
}
