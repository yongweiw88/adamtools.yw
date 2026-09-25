#' Derive pediatric LMS reference Z-scores
#'
#' Derives row-level pediatric growth Z-scores from fully prepared measurement
#' rows and a caller-supplied LMS reference table. The helper is intentionally
#' reference-driven and low level: it does not read WHO/CDC files, choose
#' parameter codes, create ADVS derived rows, round age/height coordinates,
#' convert recumbent length/standing height, derive BMI, or assign labels.
#' Study, protocol, and reference-specific preparation must happen before this
#' function is called.
#'
#' Matching uses sex, the reference coordinate (for example age, age-in-months,
#' length, height, or BMI coordinate), and optional additional `by_vars` such as
#' measurement type, age unit, length/height indicator, or reference family.
#' Reference rows must be unique for the complete matching key. Input row count
#' and order are preserved.
#'
#' The standard LMS method is
#' \deqn{z = ((x / M)^L - 1) / (S L)}
#' with the log-normal fallback
#' \deqn{z = log(x / M) / S}
#' when `abs(L) <= l_epsilon`.
#'
#' `method = "who_extended"` applies the common WHO tail extension based only
#' on the LMS-implied -3, -2, +2, and +3 SD measurement values: standard LMS is
#' used within [-3, 3], values above +3 are extended linearly from the +2/+3 SD
#' spacing, and values below -3 are extended linearly from the -2/-3 SD
#' spacing. Callers remain responsible for confirming that this documented
#' formula matches the selected WHO reference implementation and should use
#' `method = "lms"` for standard WHO or CDC LMS tables when no tail extension is
#' required.
#'
#' @param dataset Prepared measurement data frame.
#' @param reference LMS reference data frame.
#' @param value_var Measurement value column in `dataset`. Values must be
#'   numeric and positive for rows to be scored.
#' @param sex_var Sex key column in `dataset`.
#' @param coordinate_var Reference coordinate key column in `dataset`; callers
#'   must precompute any age/length/height/BMI coordinate and reference-specific
#'   rounding or conversion.
#' @param by_vars Optional additional key columns in `dataset` used for
#'   matching, for example `c("TYPE", "AGEU", "LOH")`.
#' @param ref_sex_var Sex key column in `reference`. Defaults to `sex_var`.
#' @param ref_coordinate_var Coordinate key column in `reference`. Defaults to
#'   `coordinate_var`.
#' @param ref_by_vars Optional additional key columns in `reference`, in the
#'   same order as `by_vars`. Defaults to `by_vars`.
#' @param l_var LMS L column in `reference`. Defaults to `"L"`.
#' @param m_var LMS M column in `reference`. Defaults to `"M"`.
#' @param s_var LMS S column in `reference`. Defaults to `"S"`.
#' @param z_var Output Z-score column. Defaults to `"ZSCORE"`.
#' @param method Z-score method: `"lms"` for standard LMS or
#'   `"who_extended"` for the documented WHO-style tail extension. Defaults to
#'   `"lms"`.
#' @param unmatched Policy for input rows without a matching reference row.
#'   `"error"` stops; `"NA"` returns a missing Z-score for unmatched rows.
#'   Defaults to `"error"`.
#' @param invalid Policy for invalid measurement/reference values such as
#'   missing or nonpositive `x`, missing `L`, nonpositive `M`, or nonpositive
#'   `S`. `"error"` stops; `"NA"` returns missing Z-scores for invalid rows.
#'   Defaults to `"error"`.
#' @param l_epsilon Nonnegative tolerance used to identify an approximately
#'   zero `L` requiring the log formula. Defaults to `1e-7`.
#' @param diagnostics Logical; add match diagnostics. Defaults to `FALSE`.
#' @param match_flag_var Diagnostic output column containing `"Y"` when a
#'   reference row matched and `"N"` otherwise. Used only when
#'   `diagnostics = TRUE`.
#' @param ref_coordinate_output_var Optional diagnostic output column
#'   containing the matched reference coordinate. Used only when
#'   `diagnostics = TRUE`; defaults to `"LMSCOORD"`.
#' @param overwrite Logical; allow output columns to replace existing columns
#'   in `dataset`. Defaults to `FALSE`.
#'
#' @return `dataset` with `z_var` and optional diagnostics added. Row count and
#'   row order are unchanged.
#' @export
derive_pediatric_lms_zscore <- function(dataset,
                                        reference,
                                        value_var,
                                        sex_var,
                                        coordinate_var,
                                        by_vars = NULL,
                                        ref_sex_var = sex_var,
                                        ref_coordinate_var = coordinate_var,
                                        ref_by_vars = by_vars,
                                        l_var = "L",
                                        m_var = "M",
                                        s_var = "S",
                                        z_var = "ZSCORE",
                                        method = c("lms", "who_extended"),
                                        unmatched = c("error", "NA"),
                                        invalid = c("error", "NA"),
                                        l_epsilon = 1e-7,
                                        diagnostics = FALSE,
                                        match_flag_var = "LMSMATCH",
                                        ref_coordinate_output_var = "LMSCOORD",
                                        overwrite = FALSE) {
  .adamtools_check_data_frame(dataset)
  .adamtools_check_data_frame(reference, "reference")
  .adamtools_check_scalar_name(value_var, "value_var")
  .adamtools_check_scalar_name(sex_var, "sex_var")
  .adamtools_check_scalar_name(coordinate_var, "coordinate_var")
  .adamtools_check_character_vector(by_vars, "by_vars", allow_null = TRUE)
  .adamtools_check_scalar_name(ref_sex_var, "ref_sex_var")
  .adamtools_check_scalar_name(ref_coordinate_var, "ref_coordinate_var")
  .adamtools_check_character_vector(ref_by_vars, "ref_by_vars", allow_null = TRUE)
  .adamtools_check_scalar_name(l_var, "l_var")
  .adamtools_check_scalar_name(m_var, "m_var")
  .adamtools_check_scalar_name(s_var, "s_var")
  .adamtools_check_scalar_name(z_var, "z_var")
  .adamtools_check_scalar_name(match_flag_var, "match_flag_var")
  .adamtools_check_scalar_name(ref_coordinate_output_var, "ref_coordinate_output_var", allow_null = TRUE)

  method <- match.arg(method)
  unmatched <- match.arg(unmatched)
  invalid <- match.arg(invalid)

  if (!is.logical(diagnostics) || length(diagnostics) != 1L || is.na(diagnostics)) {
    stop("`diagnostics` must be a single TRUE/FALSE value.", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("`overwrite` must be a single TRUE/FALSE value.", call. = FALSE)
  }
  if (!is.numeric(l_epsilon) || length(l_epsilon) != 1L || is.na(l_epsilon) || l_epsilon < 0) {
    stop("`l_epsilon` must be a single nonnegative number.", call. = FALSE)
  }

  by_vars <- if (is.null(by_vars)) character() else by_vars
  ref_by_vars <- if (is.null(ref_by_vars)) character() else ref_by_vars
  if (length(by_vars) != length(ref_by_vars)) {
    stop("`ref_by_vars` must have the same length and order as `by_vars`.", call. = FALSE)
  }

  data_key_vars <- c(sex_var, by_vars, coordinate_var)
  ref_key_vars <- c(ref_sex_var, ref_by_vars, ref_coordinate_var)
  if (any(duplicated(data_key_vars))) {
    stop("`sex_var`, `coordinate_var`, and `by_vars` must identify distinct dataset columns.", call. = FALSE)
  }
  if (any(duplicated(ref_key_vars))) {
    stop(
      "`ref_sex_var`, `ref_coordinate_var`, and `ref_by_vars` must identify distinct reference columns.",
      call. = FALSE
    )
  }

  .adamtools_check_columns(dataset, c(value_var, data_key_vars), "dataset")
  .adamtools_check_columns(reference, c(ref_key_vars, l_var, m_var, s_var), "reference")
  .adamtools_check_numeric_column(dataset[[value_var]], "value_var")
  .adamtools_check_numeric_column(reference[[l_var]], "l_var")
  .adamtools_check_numeric_column(reference[[m_var]], "m_var")
  .adamtools_check_numeric_column(reference[[s_var]], "s_var")

  output_columns <- z_var
  if (diagnostics) {
    output_columns <- c(output_columns, match_flag_var, ref_coordinate_output_var)
  }
  .adamtools_check_output_columns(dataset, output_columns, overwrite = overwrite)

  standard_key_vars <- paste0(".adamtools_lms_key_", seq_along(data_key_vars))
  internal_cols <- c(
    ".adamtools_lms_orig_row_",
    ".adamtools_lms_ref_row_",
    ".adamtools_lms_ref_coordinate_",
    ".adamtools_lms_l_",
    ".adamtools_lms_m_",
    ".adamtools_lms_s_",
    standard_key_vars
  )
  .adamtools_check_internal_columns(dataset, internal_cols)
  .adamtools_check_internal_columns(reference, internal_cols)

  reference_key_data <- reference[ref_key_vars]
  duplicate_ref <- duplicated(reference_key_data) | duplicated(reference_key_data, fromLast = TRUE)
  if (any(duplicate_ref)) {
    examples <- .adamtools_format_key_values(reference[duplicate_ref, , drop = FALSE], ref_key_vars)
    stop("`reference` contains duplicate LMS rows for matching key(s): ", examples, call. = FALSE)
  }

  data_keyed <- dataset
  data_keyed$.adamtools_lms_orig_row_ <- seq_len(nrow(data_keyed))
  ref_keyed <- reference
  ref_keyed$.adamtools_lms_ref_row_ <- seq_len(nrow(ref_keyed))
  ref_keyed$.adamtools_lms_ref_coordinate_ <- ref_keyed[[ref_coordinate_var]]
  ref_keyed$.adamtools_lms_l_ <- ref_keyed[[l_var]]
  ref_keyed$.adamtools_lms_m_ <- ref_keyed[[m_var]]
  ref_keyed$.adamtools_lms_s_ <- ref_keyed[[s_var]]

  for (i in seq_along(standard_key_vars)) {
    data_keyed[[standard_key_vars[i]]] <- data_keyed[[data_key_vars[i]]]
    ref_keyed[[standard_key_vars[i]]] <- ref_keyed[[ref_key_vars[i]]]
  }

  joined <- dplyr::left_join(
    data_keyed,
    ref_keyed[c(
      standard_key_vars,
      ".adamtools_lms_ref_row_",
      ".adamtools_lms_ref_coordinate_",
      ".adamtools_lms_l_",
      ".adamtools_lms_m_",
      ".adamtools_lms_s_"
    )],
    by = standard_key_vars
  )
  joined <- joined[order(joined$.adamtools_lms_orig_row_), , drop = FALSE]

  matched <- !is.na(joined$.adamtools_lms_ref_row_)
  if (any(!matched) && identical(unmatched, "error")) {
    examples <- .adamtools_format_key_values(joined[!matched, , drop = FALSE], data_key_vars)
    stop("Input row(s) have no matching LMS reference row: ", examples, call. = FALSE)
  }

  x <- joined[[value_var]]
  l <- joined$.adamtools_lms_l_
  m <- joined$.adamtools_lms_m_
  s <- joined$.adamtools_lms_s_

  invalid_measure <- is.na(x) | !is.finite(x) | x <= 0
  invalid_reference <- matched & (
    is.na(l) | !is.finite(l) |
      is.na(m) | !is.finite(m) | m <= 0 |
      is.na(s) | !is.finite(s) | s <= 0
  )
  invalid_rows <- invalid_measure | invalid_reference
  if (any(invalid_rows) && identical(invalid, "error")) {
    examples <- .adamtools_format_key_values(joined[invalid_rows, , drop = FALSE], data_key_vars)
    stop(
      "Input measurement or LMS reference values are invalid for row(s): ",
      examples,
      ". Measurement values and M/S must be positive; L, M, and S must be non-missing finite numbers.",
      call. = FALSE
    )
  }

  valid <- matched & !invalid_rows
  z <- rep(NA_real_, nrow(joined))
  z[valid] <- .adamtools_lms_standard_z(
    x = x[valid],
    l = l[valid],
    m = m[valid],
    s = s[valid],
    l_epsilon = l_epsilon
  )

  if (identical(method, "who_extended") && any(valid)) {
    z[valid] <- .adamtools_lms_who_extended_z(
      x = x[valid],
      l = l[valid],
      m = m[valid],
      s = s[valid],
      z_standard = z[valid],
      l_epsilon = l_epsilon,
      invalid = invalid
    )
  }

  nonfinite <- valid & !is.finite(z)
  if (any(nonfinite) && identical(invalid, "error")) {
    examples <- .adamtools_format_key_values(joined[nonfinite, , drop = FALSE], data_key_vars)
    stop("Z-score calculation produced non-finite values for row(s): ", examples, call. = FALSE)
  }
  z[nonfinite] <- NA_real_

  result <- dataset
  result[[z_var]] <- z
  if (diagnostics) {
    result[[match_flag_var]] <- ifelse(matched, "Y", "N")
    if (!is.null(ref_coordinate_output_var)) {
      ref_coord <- .adamtools_missing_vector_like(reference[[ref_coordinate_var]], nrow(result))
      ref_coord[matched] <- joined$.adamtools_lms_ref_coordinate_[matched]
      result[[ref_coordinate_output_var]] <- ref_coord
    }
  }

  result
}

.adamtools_check_numeric_column <- function(x, arg) {
  if (!is.numeric(x)) {
    stop("`", arg, "` must identify a numeric column.", call. = FALSE)
  }
  invisible(TRUE)
}

.adamtools_lms_standard_z <- function(x, l, m, s, l_epsilon) {
  z <- rep(NA_real_, length(x))
  near_zero <- abs(l) <= l_epsilon
  z[near_zero] <- log(x[near_zero] / m[near_zero]) / s[near_zero]
  z[!near_zero] <- ((x[!near_zero] / m[!near_zero])^l[!near_zero] - 1) /
    (s[!near_zero] * l[!near_zero])
  z
}

.adamtools_lms_who_extended_z <- function(x, l, m, s, z_standard, l_epsilon, invalid) {
  sd2pos <- .adamtools_lms_measure_at_z(2, l = l, m = m, s = s, l_epsilon = l_epsilon)
  sd3pos <- .adamtools_lms_measure_at_z(3, l = l, m = m, s = s, l_epsilon = l_epsilon)
  sd2neg <- .adamtools_lms_measure_at_z(-2, l = l, m = m, s = s, l_epsilon = l_epsilon)
  sd3neg <- .adamtools_lms_measure_at_z(-3, l = l, m = m, s = s, l_epsilon = l_epsilon)

  bad_extension <- !is.finite(sd2pos) |
    !is.finite(sd3pos) |
    !is.finite(sd2neg) |
    !is.finite(sd3neg) |
    (sd3pos - sd2pos) <= 0 |
    (sd2neg - sd3neg) <= 0
  if (any(bad_extension) && identical(invalid, "error")) {
    stop(
      "WHO extended LMS calculation has invalid +/-2 or +/-3 SD spacing. ",
      "Check that the selected reference L/M/S values support the extension formula.",
      call. = FALSE
    )
  }

  z <- z_standard
  high <- z_standard > 3 & !bad_extension
  low <- z_standard < -3 & !bad_extension
  z[high] <- 3 + (x[high] - sd3pos[high]) / (sd3pos[high] - sd2pos[high])
  z[low] <- -3 + (x[low] - sd3neg[low]) / (sd2neg[low] - sd3neg[low])
  z[bad_extension] <- NA_real_
  z
}

.adamtools_lms_measure_at_z <- function(z, l, m, s, l_epsilon) {
  out <- rep(NA_real_, length(l))
  near_zero <- abs(l) <= l_epsilon
  out[near_zero] <- m[near_zero] * exp(s[near_zero] * z)

  not_zero <- !near_zero
  base <- 1 + l[not_zero] * s[not_zero] * z
  valid_base <- base > 0
  tmp <- rep(NA_real_, length(base))
  tmp[valid_base] <- m[not_zero][valid_base] * base[valid_base]^(1 / l[not_zero][valid_base])
  out[not_zero] <- tmp
  out
}

.adamtools_missing_vector_like <- function(x, n) {
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
  if (is.integer(x)) {
    return(rep(NA_integer_, n))
  }
  if (is.numeric(x)) {
    return(rep(NA_real_, n))
  }
  if (is.logical(x)) {
    return(rep(NA, n))
  }
  rep(NA_character_, n)
}
