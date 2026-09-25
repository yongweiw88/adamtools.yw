# Internal validation helpers shared by package functions.

.adamtools_check_data_frame <- function(x, arg = "dataset") {
  if (!inherits(x, "data.frame")) {
    stop("`", arg, "` must be a data frame or tibble.", call. = FALSE)
  }
}

.adamtools_check_scalar_name <- function(x, arg, allow_null = FALSE) {
  if (allow_null && is.null(x)) {
    return(invisible(TRUE))
  }
  if (!is.character(x) || length(x) != 1L || is.na(x) || identical(x, "")) {
    stop("`", arg, "` must be a single non-missing column name.", call. = FALSE)
  }
  invisible(TRUE)
}

.adamtools_check_character_vector <- function(x, arg, allow_null = TRUE, allow_empty = FALSE) {
  if (allow_null && is.null(x)) {
    return(invisible(TRUE))
  }
  if (!is.character(x) || any(is.na(x))) {
    stop("`", arg, "` must be a character vector.", call. = FALSE)
  }
  if (!allow_empty && length(x) == 0L) {
    stop("`", arg, "` must contain at least one value.", call. = FALSE)
  }
  if (any(x == "")) {
    stop("`", arg, "` must not contain empty column names.", call. = FALSE)
  }
  invisible(TRUE)
}

.adamtools_check_columns <- function(data, columns, arg = "dataset") {
  columns <- columns[!is.na(columns) & columns != ""]
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    stop(
      "`", arg, "` is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.adamtools_check_output_columns <- function(data, columns, overwrite = FALSE) {
  columns <- columns[!is.na(columns) & columns != ""]
  duplicated_cols <- unique(columns[duplicated(columns)])
  if (length(duplicated_cols) > 0L) {
    stop(
      "Output column names must be unique; duplicate name(s): ",
      paste(duplicated_cols, collapse = ", "),
      call. = FALSE
    )
  }

  existing <- intersect(columns, names(data))
  if (length(existing) > 0L && !isTRUE(overwrite)) {
    stop(
      "Output column(s) already exist in `dataset`: ",
      paste(existing, collapse = ", "),
      ". Set `overwrite = TRUE` to replace them.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.adamtools_check_internal_columns <- function(data, columns) {
  existing <- intersect(columns, names(data))
  if (length(existing) > 0L) {
    stop(
      "Input data contains reserved internal column name(s): ",
      paste(existing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.adamtools_format_key_values <- function(data, columns, max_rows = 5L) {
  if (nrow(data) == 0L || length(columns) == 0L) {
    return("")
  }
  columns <- columns[columns %in% names(data)]
  shown <- utils::head(unique(data[columns]), max_rows)
  values <- apply(shown, 1L, function(row) {
    paste(paste0(columns, "=", as.character(row)), collapse = ", ")
  })
  paste(values, collapse = "; ")
}
