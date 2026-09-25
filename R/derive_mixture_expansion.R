#' Expand genotype/sequencing "mixture" notation rows
#'
#' Genotype and phenotype sequencing domains (SDTM `PF`/`GF`, e.g. HIV
#' resistance amino-acid substitution results) commonly report an
#' ambiguous position as a single "mixture" result like `"V381V/I"`
#' (position 381 detected both wild-type V and mutant I). The
#' corresponding ADaM output typically keeps the original mixture row AND
#' adds one additional row per individually-resolved variant (`"V381V"`,
#' `"V381I"`), so that downstream resistance-mutation matching can treat
#' each possibility independently. This formalizes that expansion, found
#' while building an ADPF program: 591 raw mixture-eligible rows (92 of
#' them true mixtures) became 784 output rows.
#'
#' @param dataset A data frame or tibble containing one row per raw
#'   result.
#' @param value_var Name of the column holding the result string (as a
#'   string, e.g. `"AVALC"`), in `PREFIX + POSITION + VARIANT[/VARIANT...]`
#'   form (e.g. `"V381V/I"`, `"K103K/N/R"`). Rows that don't match this
#'   shape are returned unchanged, with no expansion.
#' @param pattern Regex identifying a mixture value, with capture groups
#'   for the amino-acid prefix, the position digits, and the `/`-
#'   delimited variant list (in that order). Defaults to
#'   `"^([A-Za-z]+)([0-9]+)([A-Za-z]+(?:/[A-Za-z]+)+)$"`.
#'
#' @return A data frame with one row per original record (including the
#'   unmodified mixture row) plus one additional row per resolved variant
#'   for each mixture record, with `value_var` set to the resolved string
#'   (e.g. `"V381V"`) on the added rows and all other columns carried
#'   forward unchanged. Row order is not guaranteed; re-sort if needed.
#' @export
derive_mixture_expansion <- function(dataset,
                                     value_var,
                                     pattern = "^([A-Za-z]+)([0-9]+)([A-Za-z]+(?:/[A-Za-z]+)+)$") {
  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }
  if (!value_var %in% names(dataset)) {
    stop("`value_var` (\"", value_var, "\") is not a column in `dataset`.", call. = FALSE)
  }

  values <- dataset[[value_var]]
  is_mixture <- !is.na(values) & grepl(pattern, values)

  if (!any(is_mixture)) {
    return(dataset)
  }

  mixture_rows <- dataset[is_mixture, , drop = FALSE]
  matches <- regmatches(mixture_rows[[value_var]], regexpr(pattern, mixture_rows[[value_var]]))
  parts <- regmatches(mixture_rows[[value_var]], regexec(pattern, mixture_rows[[value_var]]))

  expanded <- do.call(rbind, lapply(seq_along(parts), function(i) {
    prefix <- parts[[i]][2]
    position <- parts[[i]][3]
    variants <- strsplit(parts[[i]][4], "/", fixed = TRUE)[[1]]
    row_template <- mixture_rows[i, , drop = FALSE]
    out <- row_template[rep(1, length(variants)), , drop = FALSE]
    out[[value_var]] <- paste0(prefix, position, variants)
    out
  }))

  rbind(dataset, expanded)
}
