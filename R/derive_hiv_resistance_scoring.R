#' Parse HIV resistance mutation strings
#'
#' Expands amino-acid resistance result strings such as `"K103N"` and
#' `"T66A/I/K"` to one row per parsed substitution while retaining the source
#' row identity. The parser is intentionally dictionary-version agnostic: it
#' does not contain Stanford, US-IAS, or study-specific mutation lists.
#'
#' Insertion/deletion syntax is handled through caller-overridable aliases.
#' Region mapping is also caller-overridable and is only applied when
#' `region_map` is supplied. Non-missing, non-skipped result strings that do
#' not match the supported mutation grammar stop with an error rather than
#' being silently transformed.
#'
#' @param dataset Data frame containing source mutation observations.
#' @param result_var Mutation result column in `dataset` (bare or character),
#'   for example `GFSTRESC`.
#' @param region_var Optional source region column (bare or character). When
#'   `region_map` is supplied, this column is mapped to standardized region
#'   code/name outputs.
#' @param source_id_var Output source-row identity column. Defaults to
#'   `".hiv_res_source_row"`.
#' @param region_map Optional caller-supplied region map. If supplied, it must
#'   contain `region_map_region_var`, `region_map_code_var`, and
#'   `region_map_name_var`.
#' @param region_map_region_var Region value/prefix column in `region_map`.
#' @param region_map_code_var Region code column in `region_map`.
#' @param region_map_name_var Region name column in `region_map`.
#' @param region_map_match Either `"value"` for exact matching or `"prefix"`
#'   for prefix matching. Prefix matching errors if a source value matches
#'   multiple map rows.
#' @param region_code_var Output standardized region code column.
#' @param region_name_var Output standardized region name column.
#' @param reference_aa_var Output reference amino-acid/alias column.
#' @param codon_number_var Output codon-position column.
#' @param codon_var Output codon identifier column (`reference_aa + codon`).
#' @param substitution_aa_var Output substitution amino-acid column.
#' @param substitution_sequence_var Output substitution/insertion sequence
#'   column. For slash-separated alternatives it is usually missing; for
#'   values with an underscore suffix such as `"S_ABC"` it is `"ABC"`.
#' @param mutation_var Output mutation identifier column.
#' @param result_category_var Output category column (`"INSERTION"`,
#'   `"DELETION"`, or missing).
#' @param insertion_aliases Character vector of caller-accepted insertion
#'   aliases.
#' @param deletion_aliases Character vector of caller-accepted deletion aliases.
#' @param missing_values Character vector of result values to skip. Set to
#'   `NULL` to parse all non-missing strings.
#' @param keep_missing Logical; keep one output row with parsed fields missing
#'   for missing/skipped results. Defaults to `FALSE`.
#' @param split_compact_substitutions Logical; when `TRUE`, compact mutation
#'   alternatives such as `"V11IL"` are expanded to `"V11I"` and `"V11L"`.
#'   Defaults to `FALSE` to avoid silently interpreting ambiguous source
#'   results. Stanford rule expansion uses `TRUE`.
#' @param case_sensitive Logical; when `FALSE` (default), mutation tokens and
#'   aliases are normalized to upper case before parsing and matching.
#' @param overwrite Logical; allow parser output columns to overwrite columns
#'   already present in `dataset`.
#'
#' @return A data frame containing the original source columns, source-row
#'   identity, parsed mutation variables, and optional mapped region variables.
#' @export
parse_hiv_resistance_mutations <- function(dataset,
                                           result_var,
                                           region_var = NULL,
                                           source_id_var = ".hiv_res_source_row",
                                           region_map = NULL,
                                           region_map_region_var = "REGION",
                                           region_map_code_var = "GENROICD",
                                           region_map_name_var = "GENROI",
                                           region_map_match = c("value", "prefix"),
                                           region_code_var = "GENROICD",
                                           region_name_var = "GENROI",
                                           reference_aa_var = "REFRES",
                                           codon_number_var = "GENLOC",
                                           codon_var = "CODON",
                                           substitution_aa_var = "AAS",
                                           substitution_sequence_var = "AASSEQ",
                                           mutation_var = "MUTATION",
                                           result_category_var = "RESCAT",
                                           insertion_aliases = c("INSERTION", "INS", "+"),
                                           deletion_aliases = c("DELETION", "DEL", "-"),
                                           missing_values = c("", "NA", "ND", "NR"),
                                           keep_missing = FALSE,
                                           split_compact_substitutions = FALSE,
                                           case_sensitive = FALSE,
                                           overwrite = FALSE) {
  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }
  result_name <- .hiv_res_column_name(rlang::enexpr(result_var), parent.frame(), "result_var")
  region_expr <- rlang::enexpr(region_var)
  region_name <- NULL
  if (!rlang::is_null(region_expr)) {
    region_name <- .hiv_res_column_name(region_expr, parent.frame(), "region_var")
  }
  region_map_match <- match.arg(region_map_match)

  .hiv_res_require_columns(dataset, result_name, "`dataset`")
  if (!is.null(region_name)) {
    .hiv_res_require_columns(dataset, region_name, "`dataset`")
  }
  .hiv_res_check_scalar_name(source_id_var, "source_id_var")
  .hiv_res_check_scalar_name(reference_aa_var, "reference_aa_var")
  .hiv_res_check_scalar_name(codon_number_var, "codon_number_var")
  .hiv_res_check_scalar_name(codon_var, "codon_var")
  .hiv_res_check_scalar_name(substitution_aa_var, "substitution_aa_var")
  .hiv_res_check_scalar_name(substitution_sequence_var, "substitution_sequence_var")
  .hiv_res_check_scalar_name(mutation_var, "mutation_var")
  .hiv_res_check_scalar_name(result_category_var, "result_category_var")
  if (!is.null(region_map)) {
    if (is.null(region_name)) {
      stop("`region_var` is required when `region_map` is supplied.", call. = FALSE)
    }
    .hiv_res_check_scalar_name(region_code_var, "region_code_var")
    .hiv_res_check_scalar_name(region_name_var, "region_name_var")
  }
  if (!is.logical(keep_missing) || length(keep_missing) != 1L || is.na(keep_missing)) {
    stop("`keep_missing` must be a single non-missing logical value.", call. = FALSE)
  }
  if (!is.logical(split_compact_substitutions) ||
      length(split_compact_substitutions) != 1L ||
      is.na(split_compact_substitutions)) {
    stop("`split_compact_substitutions` must be a single non-missing logical value.", call. = FALSE)
  }
  if (!is.logical(case_sensitive) || length(case_sensitive) != 1L || is.na(case_sensitive)) {
    stop("`case_sensitive` must be a single non-missing logical value.", call. = FALSE)
  }
  .hiv_res_check_aliases(insertion_aliases, "insertion_aliases")
  .hiv_res_check_aliases(deletion_aliases, "deletion_aliases")
  .hiv_res_check_alias_sets(insertion_aliases, deletion_aliases, case_sensitive)

  new_columns <- c(
    source_id_var,
    reference_aa_var,
    codon_number_var,
    codon_var,
    substitution_aa_var,
    substitution_sequence_var,
    mutation_var,
    result_category_var
  )
  if (!is.null(region_map)) {
    new_columns <- c(new_columns, region_code_var, region_name_var)
  }
  .hiv_res_check_output_columns(dataset, new_columns, overwrite = overwrite)

  source_values <- dataset[[result_name]]
  parsed <- vector("list", length(source_values))
  parse_errors <- character(0)
  skipped <- logical(length(source_values))

  normalized_missing <- .hiv_res_normalize_values(missing_values, case_sensitive)
  for (i in seq_along(source_values)) {
    raw_value <- source_values[[i]]
    is_missing <- is.na(raw_value)
    normalized_value <- .hiv_res_normalize_value(raw_value, case_sensitive)
    if (!is_missing && !is.null(normalized_missing)) {
      is_missing <- normalized_value %in% normalized_missing
    }
    if (is_missing) {
      skipped[[i]] <- TRUE
      if (keep_missing) {
        parsed[[i]] <- .hiv_res_missing_parse_row()
      }
      next
    }

    parsed[[i]] <- tryCatch(
      .hiv_res_parse_one_value(
        raw_value,
        insertion_aliases = insertion_aliases,
        deletion_aliases = deletion_aliases,
        split_compact_substitutions = split_compact_substitutions,
        case_sensitive = case_sensitive
      ),
      error = function(e) {
        parse_errors <<- c(
          parse_errors,
          paste0("row ", i, " value \"", as.character(raw_value), "\": ", conditionMessage(e))
        )
        NULL
      }
    )
  }

  if (length(parse_errors) > 0L) {
    stop(
      "Malformed HIV resistance mutation syntax; first error(s): ",
      paste(utils::head(parse_errors, 5L), collapse = " | "),
      call. = FALSE
    )
  }

  row_counts <- vapply(parsed, function(x) if (is.null(x)) 0L else nrow(x), integer(1))
  if (sum(row_counts) == 0L) {
    out <- dataset[0L, , drop = FALSE]
    out[[source_id_var]] <- integer(0)
    out[[reference_aa_var]] <- character(0)
    out[[codon_number_var]] <- character(0)
    out[[codon_var]] <- character(0)
    out[[substitution_aa_var]] <- character(0)
    out[[substitution_sequence_var]] <- character(0)
    out[[mutation_var]] <- character(0)
    out[[result_category_var]] <- character(0)
    if (!is.null(region_map)) {
      out[[region_code_var]] <- character(0)
      out[[region_name_var]] <- character(0)
    }
    return(out)
  }

  source_index <- rep(seq_along(row_counts), row_counts)
  out <- dataset[source_index, , drop = FALSE]
  row.names(out) <- NULL
  parsed_df <- do.call(rbind, parsed[row_counts > 0L])
  row.names(parsed_df) <- NULL

  out[[source_id_var]] <- source_index
  out[[reference_aa_var]] <- parsed_df$reference_aa
  out[[codon_number_var]] <- parsed_df$codon_number
  out[[codon_var]] <- parsed_df$codon
  out[[substitution_aa_var]] <- parsed_df$substitution_aa
  out[[substitution_sequence_var]] <- parsed_df$substitution_sequence
  out[[mutation_var]] <- parsed_df$mutation
  out[[result_category_var]] <- parsed_df$result_category

  if (!is.null(region_map)) {
    mapped_region <- .hiv_res_map_region(
      out[[region_name]],
      region_map = region_map,
      region_map_region_var = region_map_region_var,
      region_map_code_var = region_map_code_var,
      region_map_name_var = region_map_name_var,
      region_map_match = region_map_match
    )
    out[[region_code_var]] <- mapped_region$code
    out[[region_name_var]] <- mapped_region$name
  }

  out
}

#' Derive HIV US-IAS mutation resistance classifications
#'
#' Joins parsed observed mutations to a caller-supplied, versioned US-IAS (or
#' IAS-style) mutation dictionary. The function contains no embedded US-IAS
#' mutation list and no pre-specified mutation defaults; callers supply the
#' dictionary and any protocol-specific pre-specified mutation rules explicitly.
#'
#' Output remains mutation-level and ADGF-presentation agnostic. It includes the
#' parsed source mutation fields, IAS match/classification fields, optional
#' reference-version provenance, and optional generic `MCRIT*` variables that a
#' study program can map into ADGF parameter rows.
#'
#' @param dataset Source mutation observations.
#' @param ias_dictionary Caller-supplied IAS dictionary with at least
#'   `dictionary_genotype_var`, `dictionary_class_var`, and
#'   `dictionary_classification_var`.
#' @param result_var Mutation result column in `dataset` (bare or character).
#' @param dictionary_genotype_var Mutation/rule column in `ias_dictionary`.
#' @param key_vars Optional source key variables retained for duplicate
#'   diagnostics and downstream use.
#' @param region_var Optional source region column in `dataset`.
#' @param region_map Optional caller-supplied map for `region_var`.
#' @param region_map_region_var Region value/prefix column in `region_map`.
#' @param region_map_code_var Region code column in `region_map`.
#' @param region_map_name_var Region name column in `region_map`.
#' @param region_map_match Either `"value"` or `"prefix"` for observed region
#'   mapping.
#' @param allow_region_agnostic_matching Logical; when `FALSE` (default), a
#'   reference table containing region-coded rules requires observed region
#'   codes too, preventing cross-region false matches. Set to `TRUE` only for
#'   caller-validated region-agnostic dictionaries.
#' @param dictionary_region_code_var Dictionary standardized region code column.
#'   If absent, region is not used for dictionary matching unless supplied by
#'   `class_region_map`.
#' @param dictionary_region_name_var Optional dictionary standardized region
#'   name column.
#' @param dictionary_class_var Dictionary drug-class column.
#' @param dictionary_classification_var Dictionary mutation-classification
#'   column, for example `"SUBCLASS"`.
#' @param dictionary_drug_code_var Optional dictionary drug-code column.
#' @param dictionary_drug_name_var Optional dictionary drug-name column.
#' @param dictionary_insertion_position_regex Optional character vector of
#'   regular expressions for caller-approved IAS structural insertion-position
#'   rules. Each expression must contain a first capture group holding the codon
#'   number. For the 2024 IAS reference row `GENOTYPE = "_69"`, use
#'   `dictionary_insertion_position_regex = "^_([0-9]+)$"`. Such rules are
#'   matched only to observed parsed `INSERTION` records at the same codon and,
#'   when region-coded matching is used, the same mapped region. They never
#'   match deletions or ordinary substitutions at that codon.
#' @param unparseable_dictionary Behavior for missing/blank or malformed IAS
#'   dictionary rules after applying `dictionary_insertion_position_regex`.
#'   Defaults to `"error"`. Set to `"exclude"` only after caller review; blank
#'   rows are then excluded with an auditable warning, while nonblank malformed
#'   rows are excluded only when their row numbers are explicitly listed in
#'   `unparseable_dictionary_rows`.
#' @param unparseable_dictionary_rows Optional integer row numbers in the
#'   combined IAS rule table (the supplied `ias_dictionary` rows followed by
#'   `prespecified_rules`, when supplied) that the caller has explicitly
#'   reviewed and permits to be excluded under
#'   `unparseable_dictionary = "exclude"`. Nonblank malformed rows not listed
#'   here still error.
#' @param class_region_map Optional caller-supplied class-to-region map. If the
#'   dictionary does not contain `dictionary_region_code_var`, this map can add
#'   region code/name from `dictionary_class_var`.
#' @param class_region_map_class_var Class column in `class_region_map`.
#' @param class_region_map_code_var Region code column in `class_region_map`.
#' @param class_region_map_name_var Region name column in `class_region_map`.
#' @param prespecified_rules Optional caller-supplied pre-specified mutation
#'   rules. There is no default list. If the classification column is absent,
#'   it is filled with `prespecified_classification`.
#' @param prespecified_classification Classification value to assign to
#'   `prespecified_rules` when they do not include
#'   `dictionary_classification_var`.
#' @param major_classification_values Classification values considered "major"
#'   for generic `MCRIT*` derivation.
#' @param prespecified_classification_values Classification values considered
#'   "specified" for generic `MCRIT*` derivation.
#' @param reference_version Optional scalar dictionary version/provenance value
#'   carried into `reference_version_var`.
#' @param reference_version_var Output reference-version column name.
#' @param match_flag_var Output match flag column name.
#' @param derive_mcrit Logical; derive generic `MCRIT1`-`MCRIT3ML` variables.
#' @param mcrit_class_map Optional caller-supplied map from IAS class to the
#'   desired `MCRIT3ML` label. If omitted, `IAS_CLASS` is used as-is.
#' @param mcrit_class_map_class_var Class column in `mcrit_class_map`.
#' @param mcrit_class_map_label_var Label column in `mcrit_class_map`.
#' @param insertion_aliases,deletion_aliases,missing_values Parser controls
#'   passed to [parse_hiv_resistance_mutations()].
#' @param split_compact_dictionary_substitutions Logical; expand compact
#'   dictionary alternatives such as `"V11IL"`.
#'
#' @details
#' To use an external IAS reference such as `ias2024.xpt`, read it in the study
#' program, pass the observed study mutations in `dataset`, and supply the
#' reference-specific column mappings explicitly, for example
#' `dictionary_drug_code_var = "DRUGCODE"`,
#' `dictionary_drug_name_var = "DRUGNAME"`,
#' `class_region_map = <caller class-to-region map>`,
#' `dictionary_insertion_position_regex = "^_([0-9]+)$"`, and
#' `unparseable_dictionary = "exclude"` if the caller has reviewed and accepted
#' exclusion of known blank dictionary rows. The package does not infer or embed
#' the IAS version; callers must record the selected reference version through
#' `reference_version` or equivalent study-level provenance.
#'
#' @return A mutation-level data frame with parsed mutation fields and IAS
#'   classification outputs.
#' @export
derive_hiv_ias_resistance <- function(dataset,
                                      ias_dictionary,
                                      result_var,
                                      dictionary_genotype_var = "GENOTYPE",
                                      key_vars = NULL,
                                      region_var = NULL,
                                      region_map = NULL,
                                      region_map_region_var = "REGION",
                                      region_map_code_var = "GENROICD",
                                      region_map_name_var = "GENROI",
                                      region_map_match = c("value", "prefix"),
                                      allow_region_agnostic_matching = FALSE,
                                      dictionary_region_code_var = "GENROICD",
                                      dictionary_region_name_var = "GENROI",
                                      dictionary_class_var = "CLASS",
                                      dictionary_classification_var = "SUBCLASS",
                                      dictionary_drug_code_var = "DRUG_CODE",
                                      dictionary_drug_name_var = "DRUG_NAME",
                                      dictionary_insertion_position_regex = NULL,
                                      unparseable_dictionary = c("error", "exclude"),
                                      unparseable_dictionary_rows = NULL,
                                      class_region_map = NULL,
                                      class_region_map_class_var = "CLASS",
                                      class_region_map_code_var = "GENROICD",
                                      class_region_map_name_var = "GENROI",
                                      prespecified_rules = NULL,
                                      prespecified_classification = "PRESP",
                                      major_classification_values = "MAJOR",
                                      prespecified_classification_values = "PRESP",
                                      reference_version = NULL,
                                      reference_version_var = "REFVER",
                                      match_flag_var = "MATCHFL",
                                      derive_mcrit = TRUE,
                                      mcrit_class_map = NULL,
                                      mcrit_class_map_class_var = "CLASS",
                                      mcrit_class_map_label_var = "MCRIT3ML",
                                      insertion_aliases = c("INSERTION", "INS", "+"),
                                      deletion_aliases = c("DELETION", "DEL", "-"),
                                      missing_values = c("", "NA", "ND", "NR"),
                                      split_compact_dictionary_substitutions = FALSE) {
  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }
  if (!inherits(ias_dictionary, "data.frame")) {
    stop("`ias_dictionary` must be a data frame or tibble.", call. = FALSE)
  }
  region_map_match <- match.arg(region_map_match)
  result_name <- .hiv_res_column_name(rlang::enexpr(result_var), parent.frame(), "result_var")
  region_expr <- rlang::enexpr(region_var)
  region_name <- NULL
  if (!rlang::is_null(region_expr)) {
    region_name <- .hiv_res_column_name(region_expr, parent.frame(), "region_var")
  }
  unparseable_dictionary <- match.arg(unparseable_dictionary)
  .hiv_res_require_columns(dataset, unique(c(result_name, key_vars, region_name)), "`dataset`")
  .hiv_res_require_columns(
    ias_dictionary,
    c(dictionary_genotype_var, dictionary_class_var, dictionary_classification_var),
    "`ias_dictionary`"
  )
  .hiv_res_check_reference_version(reference_version)
  if (!is.logical(derive_mcrit) || length(derive_mcrit) != 1L || is.na(derive_mcrit)) {
    stop("`derive_mcrit` must be a single non-missing logical value.", call. = FALSE)
  }
  .hiv_res_check_logical_scalar(allow_region_agnostic_matching, "allow_region_agnostic_matching")

  obs_args <- list(
    dataset = dataset,
    result_var = result_name,
    source_id_var = ".hiv_res_source_row",
    region_map = region_map,
    region_map_region_var = region_map_region_var,
    region_map_code_var = region_map_code_var,
    region_map_name_var = region_map_name_var,
    region_map_match = region_map_match,
    insertion_aliases = insertion_aliases,
    deletion_aliases = deletion_aliases,
    missing_values = missing_values,
    keep_missing = FALSE
  )
  if (!is.null(region_name)) {
    obs_args$region_var <- region_name
  }
  obs <- do.call(parse_hiv_resistance_mutations, obs_args)

  dictionary <- .hiv_res_bind_ias_dictionary(
    ias_dictionary = ias_dictionary,
    prespecified_rules = prespecified_rules,
    dictionary_genotype_var = dictionary_genotype_var,
    dictionary_classification_var = dictionary_classification_var,
    prespecified_classification = prespecified_classification
  )
  dict_parsed <- .hiv_res_parse_ias_dictionary_rules(
    dictionary = dictionary,
    dictionary_genotype_var = dictionary_genotype_var,
    insertion_aliases = insertion_aliases,
    deletion_aliases = deletion_aliases,
    split_compact_dictionary_substitutions = split_compact_dictionary_substitutions,
    dictionary_insertion_position_regex = dictionary_insertion_position_regex,
    unparseable_dictionary = unparseable_dictionary,
    unparseable_dictionary_rows = unparseable_dictionary_rows
  )
  dict_parsed <- .hiv_res_add_class_region(
    dict_parsed,
    region_code_var = dictionary_region_code_var,
    region_name_var = dictionary_region_name_var,
    class_var = dictionary_class_var,
    class_region_map = class_region_map,
    class_region_map_class_var = class_region_map_class_var,
    class_region_map_code_var = class_region_map_code_var,
    class_region_map_name_var = class_region_map_name_var,
    caller = "`ias_dictionary`"
  )

  region_join <- .hiv_res_align_region_join(
    observed = obs,
    reference = dict_parsed,
    observed_region_code_var = "GENROICD",
    reference_region_code_var = dictionary_region_code_var,
    allow_region_agnostic_matching = allow_region_agnostic_matching,
    context = "`ias_dictionary`"
  )
  obs <- region_join$observed
  dict_parsed <- region_join$reference
  join_keys <- region_join$join_keys
  dict_parsed <- .hiv_res_expand_ias_structural_rules(
    dict_parsed = dict_parsed,
    observed = obs,
    join_keys = join_keys
  )
  dict_lookup <- .hiv_res_ias_lookup(
    dict_parsed = dict_parsed,
    join_keys = join_keys,
    dictionary_genotype_var = dictionary_genotype_var,
    dictionary_class_var = dictionary_class_var,
    dictionary_classification_var = dictionary_classification_var,
    dictionary_drug_code_var = dictionary_drug_code_var,
    dictionary_drug_name_var = dictionary_drug_name_var
  )

  result <- merge(obs, dict_lookup, by = join_keys, all.x = TRUE, sort = FALSE)
  result <- result[order(result$.hiv_res_source_row), , drop = FALSE]
  row.names(result) <- NULL
  result[[match_flag_var]] <- ifelse(!is.na(result$IAS_CLASSIFICATION), "Y", NA_character_)
  if (!is.null(reference_version)) {
    result[[reference_version_var]] <- reference_version
  }

  if (derive_mcrit) {
    result <- .hiv_res_add_ias_mcrit(
      result,
      dict_lookup = dict_lookup,
      join_keys = join_keys,
      major_classification_values = major_classification_values,
      prespecified_classification_values = prespecified_classification_values,
      mcrit_class_map = mcrit_class_map,
      mcrit_class_map_class_var = mcrit_class_map_class_var,
      mcrit_class_map_label_var = mcrit_class_map_label_var
    )
  }

  result
}

#' Derive HIV Stanford genotypic susceptibility penalty scores
#'
#' Derives per-key, per-drug Stanford-style HIV genotypic susceptibility
#' penalty scores from observed mutation strings and a caller-supplied
#' versioned reference table. The function supports single-mutation rules,
#' multi-alternative rules, and combination rules separated by `"+"`. A
#' combination rule contributes its penalty only when every component is
#' observed. Single-mutation penalties are reduced to the maximum penalty per
#' codon per drug before summing; applicable combination penalties are added;
#' the final score is floored at zero.
#'
#' The function does not embed Stanford score tables, drug lists, regimen lists,
#' or susceptibility category bands. Supply `drug_reference` to control the
#' emitted drug/reference combinations, `regimen_drugs` to derive a caller-
#' specified total, and `category_rules` to categorize the derived numeric score
#' with [derive_hiv_resistance_category()].
#'
#' @param dataset Source mutation observations.
#' @param stanford_rules Caller-supplied Stanford reference rules.
#' @param result_var Mutation result column in `dataset` (bare or character).
#' @param key_vars Character vector identifying the subject/assessment grain for
#'   scoring.
#' @param rule_var Rule/mutation column in `stanford_rules`; combination
#'   components are separated by `"+"`.
#' @param drug_var Drug-code column in `stanford_rules` and `drug_reference`.
#' @param score_var Numeric penalty-score column in `stanford_rules`.
#' @param region_var Optional source region column in `dataset`.
#' @param region_map Optional caller-supplied map for `region_var`.
#' @param region_map_region_var,region_map_code_var,region_map_name_var Region
#'   map column names for observed mutations.
#' @param region_map_match Either `"value"` or `"prefix"` for observed region
#'   mapping.
#' @param allow_region_agnostic_matching Logical; when `FALSE` (default),
#'   region-coded Stanford rules require observed region codes too. Set to
#'   `TRUE` only for caller-validated region-agnostic scoring.
#' @param rule_region_code_var Region code column in `stanford_rules`; if absent
#'   it can be supplied through `class_region_map`.
#' @param rule_region_name_var Optional region name column in `stanford_rules`.
#' @param rule_class_var Optional rule drug-class column.
#' @param class_region_map Optional caller-supplied class-to-region map used
#'   when `stanford_rules` does not already include `rule_region_code_var`.
#' @param class_region_map_class_var,class_region_map_code_var,class_region_map_name_var
#'   Column names in `class_region_map`.
#' @param drug_reference Optional data frame defining every drug/reference
#'   combination to emit. It must include `drug_var`; when omitted, distinct
#'   drug/reference combinations are taken from `stanford_rules`.
#' @param drug_name_var Optional drug-name column to carry from rules/reference.
#' @param reference_version Optional scalar dictionary version/provenance value
#'   carried into `reference_version_var`.
#' @param reference_version_var Output reference-version column name.
#' @param penalty_score_var Output total penalty-score column name.
#' @param single_score_var Output single-rule component score column name.
#' @param combination_score_var Output combination-rule component score column
#'   name.
#' @param category_rules Optional caller-supplied category rules passed to
#'   [derive_hiv_resistance_category()] with `penalty_score_var` as the numeric
#'   value.
#' @param category_match_vars Optional match variables for category rules.
#' @param category_var,ordinal_var Output category and ordinal columns.
#' @param category_unmatched Behavior for uncategorized scores: `"error"` or
#'   `"na"`.
#' @param regimen_drugs Optional character vector of drug codes to sum into a
#'   total row. Defaults to `NULL` (no regimen total).
#' @param total_drug_code,total_drug_name Output drug code/name for the regimen
#'   total row.
#' @param duplicate_mutation_handling Behavior when duplicate parsed mutations
#'   occur within a scoring key: `"error"` (default) or `"distinct"`.
#' @param insertion_aliases,deletion_aliases,missing_values Parser controls
#'   passed to [parse_hiv_resistance_mutations()].
#'
#' @return A data frame with one row per `key_vars` by emitted drug/reference
#'   combination, optional regimen total rows, numeric scores, optional
#'   categories, and optional reference-version provenance.
#' @export
derive_hiv_stanford_gss <- function(dataset,
                                    stanford_rules,
                                    result_var,
                                    key_vars,
                                    rule_var = "GENOTYPE",
                                    drug_var = "DRUG_CODE",
                                    score_var = "GSS",
                                    region_var = NULL,
                                    region_map = NULL,
                                    region_map_region_var = "REGION",
                                    region_map_code_var = "GENROICD",
                                    region_map_name_var = "GENROI",
                                    region_map_match = c("value", "prefix"),
                                    allow_region_agnostic_matching = FALSE,
                                    rule_region_code_var = "GENROICD",
                                    rule_region_name_var = "GENROI",
                                    rule_class_var = "CLASS",
                                    class_region_map = NULL,
                                    class_region_map_class_var = "CLASS",
                                    class_region_map_code_var = "GENROICD",
                                    class_region_map_name_var = "GENROI",
                                    drug_reference = NULL,
                                    drug_name_var = "DRUG",
                                    reference_version = NULL,
                                    reference_version_var = "REFVER",
                                    penalty_score_var = "GSS",
                                    single_score_var = "GSS_SINGLE",
                                    combination_score_var = "GSS_COMBINATION",
                                    category_rules = NULL,
                                    category_match_vars = NULL,
                                    category_var = "AVALCAT1",
                                    ordinal_var = "AVALCAT1N",
                                    category_unmatched = c("error", "na"),
                                    regimen_drugs = NULL,
                                    total_drug_code = "TOTAL",
                                    total_drug_name = "Total Genotypic Susceptibility Score",
                                    duplicate_mutation_handling = c("error", "distinct"),
                                    insertion_aliases = c("INSERTION", "INS", "+"),
                                    deletion_aliases = c("DELETION", "DEL", "-"),
                                    missing_values = c("", "NA", "ND", "NR")) {
  if (!inherits(dataset, "data.frame")) {
    stop("`dataset` must be a data frame or tibble.", call. = FALSE)
  }
  if (!inherits(stanford_rules, "data.frame")) {
    stop("`stanford_rules` must be a data frame or tibble.", call. = FALSE)
  }
  region_map_match <- match.arg(region_map_match)
  category_unmatched <- match.arg(category_unmatched)
  duplicate_mutation_handling <- match.arg(duplicate_mutation_handling)
  result_name <- .hiv_res_column_name(rlang::enexpr(result_var), parent.frame(), "result_var")
  region_expr <- rlang::enexpr(region_var)
  region_name <- NULL
  if (!rlang::is_null(region_expr)) {
    region_name <- .hiv_res_column_name(region_expr, parent.frame(), "region_var")
  }
  .hiv_res_check_character_vector(key_vars, "key_vars", allow_null = FALSE)
  .hiv_res_require_columns(dataset, unique(c(result_name, key_vars, region_name)), "`dataset`")
  .hiv_res_require_columns(stanford_rules, c(rule_var, drug_var, score_var), "`stanford_rules`")
  if (!is.numeric(stanford_rules[[score_var]])) {
    stop("`score_var` must identify a numeric column in `stanford_rules`.", call. = FALSE)
  }
  if (any(is.na(stanford_rules[[score_var]]) | !is.finite(stanford_rules[[score_var]]))) {
    stop("`stanford_rules` contains missing or non-finite penalty scores.", call. = FALSE)
  }
  .hiv_res_check_reference_version(reference_version)
  .hiv_res_check_logical_scalar(allow_region_agnostic_matching, "allow_region_agnostic_matching")

  rules <- .hiv_res_add_class_region(
    stanford_rules,
    region_code_var = rule_region_code_var,
    region_name_var = rule_region_name_var,
    class_var = rule_class_var,
    class_region_map = class_region_map,
    class_region_map_class_var = class_region_map_class_var,
    class_region_map_code_var = class_region_map_code_var,
    class_region_map_name_var = class_region_map_name_var,
    caller = "`stanford_rules`"
  )
  rule_components <- .hiv_res_expand_stanford_rules(
    rules = rules,
    rule_var = rule_var,
    drug_var = drug_var,
    score_var = score_var,
    insertion_aliases = insertion_aliases,
    deletion_aliases = deletion_aliases
  )

  obs_args <- list(
    dataset = dataset,
    result_var = result_name,
    source_id_var = ".hiv_res_source_row",
    region_map = region_map,
    region_map_region_var = region_map_region_var,
    region_map_code_var = region_map_code_var,
    region_map_name_var = region_map_name_var,
    region_map_match = region_map_match,
    insertion_aliases = insertion_aliases,
    deletion_aliases = deletion_aliases,
    missing_values = missing_values,
    keep_missing = FALSE
  )
  if (!is.null(region_name)) {
    obs_args$region_var <- region_name
  }
  obs <- do.call(parse_hiv_resistance_mutations, obs_args)

  region_join <- .hiv_res_align_region_join(
    observed = obs,
    reference = rule_components,
    observed_region_code_var = "GENROICD",
    reference_region_code_var = rule_region_code_var,
    allow_region_agnostic_matching = allow_region_agnostic_matching,
    context = "`stanford_rules`"
  )
  obs <- region_join$observed
  rule_components <- region_join$reference
  join_keys <- region_join$join_keys

  obs_unique <- .hiv_res_observed_scoring_mutations(
    obs = obs,
    key_vars = key_vars,
    join_keys = join_keys,
    duplicate_mutation_handling = duplicate_mutation_handling
  )
  rule_components <- .hiv_res_validate_gss_rules(rule_components, join_keys)

  single_scores <- .hiv_res_score_stanford_single(
    obs_unique = obs_unique,
    rule_components = rule_components,
    join_keys = join_keys,
    key_vars = key_vars
  )
  combo_scores <- .hiv_res_score_stanford_combo(
    obs_unique = obs_unique,
    rule_components = rule_components,
    join_keys = join_keys,
    key_vars = key_vars
  )

  drug_ref <- .hiv_res_gss_drug_reference(
    rules = rules,
    drug_reference = drug_reference,
    drug_var = drug_var,
    drug_name_var = drug_name_var,
    rule_class_var = rule_class_var,
    rule_region_code_var = rule_region_code_var,
    rule_region_name_var = rule_region_name_var
  )
  result <- .hiv_res_build_gss_output(
    dataset = dataset,
    key_vars = key_vars,
    drug_ref = drug_ref,
    drug_var = drug_var,
    single_scores = single_scores,
    combo_scores = combo_scores,
    penalty_score_var = penalty_score_var,
    single_score_var = single_score_var,
    combination_score_var = combination_score_var
  )

  if (!is.null(regimen_drugs)) {
    result <- .hiv_res_add_regimen_total(
      result = result,
      key_vars = key_vars,
      drug_var = drug_var,
      drug_name_var = drug_name_var,
      regimen_drugs = regimen_drugs,
      total_drug_code = total_drug_code,
      total_drug_name = total_drug_name,
      penalty_score_var = penalty_score_var,
      single_score_var = single_score_var,
      combination_score_var = combination_score_var
    )
  }

  if (!is.null(reference_version)) {
    result[[reference_version_var]] <- reference_version
  }

  if (!is.null(category_rules)) {
    result <- derive_hiv_resistance_category(
      result,
      category_rules,
      value_var = penalty_score_var,
      match_vars = category_match_vars,
      category_var = category_var,
      ordinal_var = ordinal_var,
      unmatched = category_unmatched
    )
  }

  result
}

.hiv_res_parse_one_value <- function(value,
                                     insertion_aliases,
                                     deletion_aliases,
                                     split_compact_substitutions,
                                     case_sensitive) {
  x <- .hiv_res_normalize_value(value, case_sensitive)
  x <- gsub("[[:space:]]+", "", x)
  if (!nzchar(x)) {
    stop("empty mutation value", call. = FALSE)
  }
  digit_pos <- regexpr("[0-9]", x)
  if (digit_pos[[1]] < 1L) {
    stop("no codon number found", call. = FALSE)
  }
  reference_aa <- substr(x, 1L, digit_pos[[1]] - 1L)
  if (!nzchar(reference_aa)) {
    stop("missing reference amino acid or insertion/deletion alias", call. = FALSE)
  }
  remainder <- substr(x, digit_pos[[1]], nchar(x))
  match <- regexec("^([0-9]+)(.*)$", remainder)
  pieces <- regmatches(remainder, match)[[1]]
  if (length(pieces) != 3L) {
    stop("codon number is malformed", call. = FALSE)
  }
  codon_number <- pieces[[2]]
  tail <- pieces[[3]]
  prefix_category <- .hiv_res_alias_category(reference_aa, insertion_aliases, deletion_aliases, case_sensitive)
  if (is.na(prefix_category) && !grepl("^[A-Z*?]$", reference_aa)) {
    stop("reference amino acid must be a single-letter amino-acid code or a configured alias", call. = FALSE)
  }
  if (!nzchar(tail) && is.na(prefix_category)) {
    stop("missing substitution amino acid", call. = FALSE)
  }

  if (nzchar(tail) && (startsWith(tail, "/") || endsWith(tail, "/") || grepl("//", tail, fixed = TRUE))) {
    stop("empty substitution alternative", call. = FALSE)
  }
  alternatives <- if (nzchar(tail)) strsplit(tail, "/", fixed = TRUE)[[1]] else ""
  alternatives <- trimws(alternatives)
  if (!case_sensitive) {
    alternatives <- toupper(alternatives)
  }
  if (any(!nzchar(alternatives)) && is.na(prefix_category)) {
    stop("empty substitution alternative", call. = FALSE)
  }

  rows <- vector("list", length(alternatives))
  out_index <- 0L
  for (alternative in alternatives) {
    parsed_alt <- .hiv_res_parse_alternative(
      alternative = alternative,
      reference_aa = reference_aa,
      codon_number = codon_number,
      prefix_category = prefix_category,
      insertion_aliases = insertion_aliases,
      deletion_aliases = deletion_aliases,
      split_compact_substitutions = split_compact_substitutions,
      case_sensitive = case_sensitive
    )
    for (j in seq_len(nrow(parsed_alt))) {
      out_index <- out_index + 1L
      rows[[out_index]] <- parsed_alt[j, , drop = FALSE]
    }
  }
  do.call(rbind, rows[seq_len(out_index)])
}

.hiv_res_parse_alternative <- function(alternative,
                                       reference_aa,
                                       codon_number,
                                       prefix_category,
                                       insertion_aliases,
                                       deletion_aliases,
                                       split_compact_substitutions,
                                       case_sensitive) {
  alias_info <- .hiv_res_extract_alias(
    alternative,
    insertion_aliases = insertion_aliases,
    deletion_aliases = deletion_aliases,
    case_sensitive = case_sensitive
  )
  category <- prefix_category
  if (!is.na(alias_info$category)) {
    if (!is.na(category) && category != alias_info$category) {
      stop("conflicting insertion/deletion aliases", call. = FALSE)
    }
    category <- alias_info$category
  }
  token <- alias_info$remainder
  if (!nzchar(token) && is.na(category)) {
    stop("missing substitution amino acid", call. = FALSE)
  }

  sequence <- NA_character_
  if (grepl("_", token, fixed = TRUE)) {
    token_parts <- strsplit(token, "_", fixed = TRUE)[[1]]
    token <- token_parts[[1]]
    sequence <- paste(token_parts[-1L], collapse = "_")
    if (!nzchar(sequence)) {
      stop("empty substitution sequence after underscore", call. = FALSE)
    }
  }
  if (identical(category, "DELETION") && !nzchar(token)) {
    token <- NA_character_
  }
  if (identical(category, "INSERTION") && !nzchar(token)) {
    token <- NA_character_
  }

  if (!is.na(token) && nzchar(token)) {
    if (!grepl("^[A-Z*?]+$", token)) {
      stop("substitution amino acid contains unsupported characters", call. = FALSE)
    }
    if (is.na(category) && nchar(token) > 1L && !split_compact_substitutions) {
      stop(
        "multi-character substitution is ambiguous; use slash-separated alternatives ",
        "or set `split_compact_substitutions = TRUE` for reference-rule expansion",
        call. = FALSE
      )
    }
  }

  substitutions <- token
  if (is.na(category) && !is.na(token) && split_compact_substitutions && nchar(token) > 1L) {
    substitutions <- strsplit(token, "", fixed = TRUE)[[1]]
    sequence <- token
  }
  if (length(substitutions) == 0L) {
    substitutions <- NA_character_
  }

  result <- data.frame(
    reference_aa = rep(reference_aa, length(substitutions)),
    codon_number = rep(codon_number, length(substitutions)),
    substitution_aa = substitutions,
    substitution_sequence = rep(sequence, length(substitutions)),
    result_category = rep(ifelse(is.na(category), NA_character_, category), length(substitutions)),
    stringsAsFactors = FALSE
  )
  result$codon <- paste0(result$reference_aa, result$codon_number)
  result$mutation <- .hiv_res_build_mutation_id(
    reference_aa = result$reference_aa,
    codon_number = result$codon_number,
    substitution_aa = result$substitution_aa,
    result_category = result$result_category
  )
  result[, c(
    "reference_aa",
    "codon_number",
    "codon",
    "substitution_aa",
    "substitution_sequence",
    "mutation",
    "result_category"
  ), drop = FALSE]
}

.hiv_res_build_mutation_id <- function(reference_aa, codon_number, substitution_aa, result_category) {
  suffix <- substitution_aa
  suffix[is.na(suffix) & result_category == "INSERTION"] <- "INS"
  suffix[is.na(suffix) & result_category == "DELETION"] <- "DEL"
  paste0(reference_aa, codon_number, suffix)
}

.hiv_res_missing_parse_row <- function() {
  data.frame(
    reference_aa = NA_character_,
    codon_number = NA_character_,
    codon = NA_character_,
    substitution_aa = NA_character_,
    substitution_sequence = NA_character_,
    mutation = NA_character_,
    result_category = NA_character_,
    stringsAsFactors = FALSE
  )
}

.hiv_res_bind_ias_dictionary <- function(ias_dictionary,
                                         prespecified_rules,
                                         dictionary_genotype_var,
                                         dictionary_classification_var,
                                         prespecified_classification) {
  dictionary <- as.data.frame(ias_dictionary, stringsAsFactors = FALSE)
  dictionary$.hiv_res_rule_source <- "DICTIONARY"
  if (is.null(prespecified_rules)) {
    return(dictionary)
  }
  if (!inherits(prespecified_rules, "data.frame")) {
    stop("`prespecified_rules` must be a data frame or tibble.", call. = FALSE)
  }
  .hiv_res_require_columns(prespecified_rules, dictionary_genotype_var, "`prespecified_rules`")
  prespecified <- as.data.frame(prespecified_rules, stringsAsFactors = FALSE)
  if (!dictionary_classification_var %in% names(prespecified)) {
    prespecified[[dictionary_classification_var]] <- prespecified_classification
  }
  prespecified$.hiv_res_rule_source <- "PRESPECIFIED"
  dplyr::bind_rows(dictionary, prespecified)
}

.hiv_res_parse_ias_dictionary_rules <- function(dictionary,
                                                dictionary_genotype_var,
                                                insertion_aliases,
                                                deletion_aliases,
                                                split_compact_dictionary_substitutions,
                                                dictionary_insertion_position_regex,
                                                unparseable_dictionary,
                                                unparseable_dictionary_rows) {
  .hiv_res_check_structural_regex(dictionary_insertion_position_regex)
  .hiv_res_check_unparseable_rows(
    unparseable_dictionary_rows = unparseable_dictionary_rows,
    n_rules = nrow(dictionary),
    unparseable_dictionary = unparseable_dictionary
  )

  rule_values <- dictionary[[dictionary_genotype_var]]
  parsed_rows <- vector("list", nrow(dictionary))
  parsed_count <- 0L
  excluded <- data.frame(
    row = integer(0),
    genotype = character(0),
    reason = character(0),
    stringsAsFactors = FALSE
  )
  errors <- character(0)

  for (i in seq_len(nrow(dictionary))) {
    raw_value <- rule_values[[i]]
    normalized_value <- .hiv_res_normalize_value(raw_value, case_sensitive = FALSE)
    display_value <- if (is.na(raw_value)) NA_character_ else as.character(raw_value)
    is_blank <- is.na(raw_value) || !nzchar(normalized_value)

    if (is_blank) {
      reason <- "missing or empty rule value"
      if (unparseable_dictionary == "exclude") {
        excluded <- rbind(
          excluded,
          data.frame(row = i, genotype = display_value, reason = reason, stringsAsFactors = FALSE)
        )
        next
      }
      errors <- c(
        errors,
        paste0("row ", i, " genotype ", .hiv_res_quote_value(display_value), ": ", reason)
      )
      next
    }

    structural_codon <- .hiv_res_match_insertion_position_rule(
      value = normalized_value,
      dictionary_insertion_position_regex = dictionary_insertion_position_regex
    )
    if (!is.na(structural_codon)) {
      parsed_count <- parsed_count + 1L
      parsed_rows[[parsed_count]] <- .hiv_res_ias_structural_rule_row(
        dictionary = dictionary,
        row_index = i,
        codon_number = structural_codon,
        rule_value = normalized_value
      )
      next
    }

    parsed_value <- tryCatch(
      .hiv_res_parse_one_value(
        raw_value,
        insertion_aliases = insertion_aliases,
        deletion_aliases = deletion_aliases,
        split_compact_substitutions = split_compact_dictionary_substitutions,
        case_sensitive = FALSE
      ),
      error = function(e) {
        e
      }
    )

    if (inherits(parsed_value, "error")) {
      reason <- conditionMessage(parsed_value)
      explicit_exclusion <- !is.null(unparseable_dictionary_rows) && i %in% unparseable_dictionary_rows
      if (unparseable_dictionary == "exclude" && explicit_exclusion) {
        excluded <- rbind(
          excluded,
          data.frame(row = i, genotype = display_value, reason = reason, stringsAsFactors = FALSE)
        )
        next
      }
      errors <- c(
        errors,
        paste0("row ", i, " genotype ", .hiv_res_quote_value(display_value), ": ", reason)
      )
      next
    }

    parsed_count <- parsed_count + 1L
    parsed_rows[[parsed_count]] <- .hiv_res_ias_parsed_rule_rows(
      dictionary = dictionary,
      row_index = i,
      parsed = parsed_value
    )
  }

  if (length(errors) > 0L) {
    if (unparseable_dictionary == "exclude") {
      stop(
        "`ias_dictionary` contains unparseable nonblank rule(s) that were not explicitly listed in ",
        "`unparseable_dictionary_rows`; first error(s): ",
        paste(utils::head(errors, 5L), collapse = " | "),
        call. = FALSE
      )
    }
    stop(
      "`ias_dictionary` contains unparseable rule(s); first error(s): ",
      paste(utils::head(errors, 5L), collapse = " | "),
      call. = FALSE
    )
  }

  excluded_rows <- excluded$row
  requested_exclusions <- if (is.null(unparseable_dictionary_rows)) integer(0) else unparseable_dictionary_rows
  unused_exclusions <- setdiff(requested_exclusions, excluded_rows)
  if (length(unused_exclusions) > 0L) {
    stop(
      "`unparseable_dictionary_rows` includes row(s) that were parseable or handled structurally: ",
      paste(unused_exclusions, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(excluded) > 0L) {
    warning(
      "Excluded unparseable `ias_dictionary` row(s): ",
      paste(.hiv_res_format_excluded_rows(excluded), collapse = "; "),
      call. = FALSE
    )
  }

  if (parsed_count == 0L) {
    stop("`ias_dictionary` produced no parseable mutation rules.", call. = FALSE)
  }

  out <- do.call(rbind, parsed_rows[seq_len(parsed_count)])
  row.names(out) <- NULL
  out
}

.hiv_res_ias_parsed_rule_rows <- function(dictionary, row_index, parsed) {
  out <- dictionary[rep(row_index, nrow(parsed)), , drop = FALSE]
  row.names(out) <- NULL
  out$.hiv_res_rule_row <- row_index
  out$.hiv_res_structural_rule <- FALSE
  out$REFRES <- parsed$reference_aa
  out$GENLOC <- parsed$codon_number
  out$CODON <- parsed$codon
  out$AAS <- parsed$substitution_aa
  out$AASSEQ <- parsed$substitution_sequence
  out$MUTATION <- parsed$mutation
  out$RESCAT <- parsed$result_category
  out
}

.hiv_res_ias_structural_rule_row <- function(dictionary, row_index, codon_number, rule_value) {
  out <- dictionary[row_index, , drop = FALSE]
  row.names(out) <- NULL
  out$.hiv_res_rule_row <- row_index
  out$.hiv_res_structural_rule <- TRUE
  out$REFRES <- NA_character_
  out$GENLOC <- codon_number
  out$CODON <- NA_character_
  out$AAS <- NA_character_
  out$AASSEQ <- NA_character_
  out$MUTATION <- rule_value
  out$RESCAT <- "INSERTION"
  out
}

.hiv_res_expand_ias_structural_rules <- function(dict_parsed, observed, join_keys) {
  if (!".hiv_res_structural_rule" %in% names(dict_parsed) ||
      !any(dict_parsed$.hiv_res_structural_rule)) {
    return(dict_parsed)
  }

  normal <- dict_parsed[!dict_parsed$.hiv_res_structural_rule, , drop = FALSE]
  structural <- dict_parsed[dict_parsed$.hiv_res_structural_rule, , drop = FALSE]
  expanded <- vector("list", nrow(structural))
  expanded_count <- 0L

  for (i in seq_len(nrow(structural))) {
    candidate_index <- !is.na(observed$GENLOC) &
      observed$GENLOC == structural$GENLOC[[i]] &
      !is.na(observed$RESCAT) &
      observed$RESCAT == structural$RESCAT[[i]]
    candidates <- observed[candidate_index, , drop = FALSE]
    if ("GENROICD" %in% join_keys && "GENROICD" %in% names(structural)) {
      candidates <- candidates[
        !is.na(candidates$GENROICD) &
          !is.na(structural$GENROICD[[i]]) &
          candidates$GENROICD == structural$GENROICD[[i]],
        ,
        drop = FALSE
      ]
    }
    if (nrow(candidates) == 0L) {
      next
    }

    piece <- structural[rep(i, nrow(candidates)), , drop = FALSE]
    row.names(piece) <- NULL
    piece$REFRES <- candidates$REFRES
    piece$GENLOC <- candidates$GENLOC
    piece$CODON <- candidates$CODON
    piece$AAS <- candidates$AAS
    piece$AASSEQ <- candidates$AASSEQ
    piece$MUTATION <- candidates$MUTATION
    piece$RESCAT <- candidates$RESCAT
    expanded_count <- expanded_count + 1L
    expanded[[expanded_count]] <- piece
  }

  if (expanded_count == 0L) {
    return(normal)
  }
  out <- rbind(normal, do.call(rbind, expanded[seq_len(expanded_count)]))
  row.names(out) <- NULL
  out
}

.hiv_res_ias_lookup <- function(dict_parsed,
                                join_keys,
                                dictionary_genotype_var,
                                dictionary_class_var,
                                dictionary_classification_var,
                                dictionary_drug_code_var,
                                dictionary_drug_name_var) {
  optional_cols <- c(dictionary_drug_code_var, dictionary_drug_name_var)
  optional_cols <- optional_cols[optional_cols %in% names(dict_parsed)]
  lookup_cols <- unique(c(
    join_keys,
    dictionary_genotype_var,
    dictionary_class_var,
    dictionary_classification_var,
    optional_cols,
    ".hiv_res_rule_source"
  ))
  lookup <- unique(dict_parsed[, lookup_cols, drop = FALSE])

  ambiguity_key_cols <- join_keys
  if (length(optional_cols) > 0L) {
    ambiguity_key_cols <- unique(c(ambiguity_key_cols, optional_cols))
  }
  ambiguity_cols <- setdiff(lookup_cols, c(ambiguity_key_cols, ".hiv_res_rule_source"))
  .hiv_res_error_on_ambiguous_reference(
    lookup,
    key_cols = ambiguity_key_cols,
    value_cols = ambiguity_cols,
    context = "`ias_dictionary`"
  )
  dedupe_cols <- setdiff(names(lookup), ".hiv_res_rule_source")
  lookup <- lookup[!duplicated(lookup[dedupe_cols]), , drop = FALSE]

  names(lookup)[names(lookup) == dictionary_genotype_var] <- "IAS_GENOTYPE"
  names(lookup)[names(lookup) == dictionary_class_var] <- "IAS_CLASS"
  names(lookup)[names(lookup) == dictionary_classification_var] <- "IAS_CLASSIFICATION"
  if (dictionary_drug_code_var %in% names(lookup)) {
    names(lookup)[names(lookup) == dictionary_drug_code_var] <- "IAS_DRUG_CODE"
  }
  if (dictionary_drug_name_var %in% names(lookup)) {
    names(lookup)[names(lookup) == dictionary_drug_name_var] <- "IAS_DRUG_NAME"
  }
  lookup
}

.hiv_res_add_ias_mcrit <- function(result,
                                   dict_lookup,
                                   join_keys,
                                   major_classification_values,
                                   prespecified_classification_values,
                                   mcrit_class_map,
                                   mcrit_class_map_class_var,
                                   mcrit_class_map_label_var) {
  major_values <- toupper(as.character(major_classification_values))
  prespecified_values <- toupper(as.character(prespecified_classification_values))
  subclass <- toupper(as.character(result$IAS_CLASSIFICATION))
  is_major <- !is.na(subclass) & subclass %in% major_values
  is_prespecified <- !is.na(subclass) & subclass %in% prespecified_values
  is_active <- is_major | is_prespecified

  result$MCRIT1 <- ifelse(is_active, "Any Mutation", NA_character_)
  result$MCRIT2 <- ifelse(
    is_prespecified,
    "Specified Mutation",
    ifelse(is_major, "Major Mutation", NA_character_)
  )
  result$MCRIT3 <- ifelse(
    is_prespecified,
    "Specified Mutation Class",
    ifelse(is_major, "Major Mutation Class", NA_character_)
  )
  result$MCRIT2ML <- ifelse(is_active, result$MUTATION, NA_character_)
  result$MCRIT3ML <- ifelse(is_active, .hiv_res_map_mcrit_class(result$IAS_CLASS, mcrit_class_map, mcrit_class_map_class_var, mcrit_class_map_label_var), NA_character_)

  active_lookup <- dict_lookup[!is.na(dict_lookup$IAS_CLASSIFICATION) &
    toupper(as.character(dict_lookup$IAS_CLASSIFICATION)) %in% c(major_values, prespecified_values), , drop = FALSE]
  rule_labels <- .hiv_res_ias_rule_labels(active_lookup, join_keys)
  if (nrow(rule_labels) > 0L) {
    label_key <- .hiv_res_key(rule_labels, join_keys)
    result_key <- .hiv_res_key(result, join_keys)
    result$MCRIT1ML <- rule_labels$.hiv_res_ias_rule[match(result_key, label_key)]
    result$MCRIT1ML[!is_active] <- NA_character_
  } else {
    result$MCRIT1ML <- NA_character_
  }

  result
}

.hiv_res_ias_rule_labels <- function(active_lookup, join_keys) {
  if (nrow(active_lookup) == 0L) {
    return(data.frame())
  }
  group_keys <- intersect(c("GENROICD", "CODON", "RESCAT"), join_keys)
  if (!"CODON" %in% group_keys) {
    group_keys <- c(group_keys, "CODON")
  }
  group_id <- .hiv_res_key(active_lookup, group_keys)
  groups <- split(seq_len(nrow(active_lookup)), group_id)
  out <- vector("list", length(groups))
  i <- 0L
  for (idx in groups) {
    i <- i + 1L
    row <- active_lookup[idx[[1]], group_keys, drop = FALSE]
    aas <- active_lookup$AAS[idx]
    aas <- ifelse(is.na(aas) & active_lookup$RESCAT[idx] == "DELETION", "DEL", aas)
    aas <- ifelse(is.na(aas) & active_lookup$RESCAT[idx] == "INSERTION", "INS", aas)
    aas <- unique(aas[!is.na(aas) & nzchar(aas)])
    row$.hiv_res_ias_rule <- paste0(active_lookup$CODON[idx[[1]]], paste(aas, collapse = "/"))
    out[[i]] <- row
  }
  do.call(rbind, out)
}

.hiv_res_expand_stanford_rules <- function(rules,
                                           rule_var,
                                           drug_var,
                                           score_var,
                                           insertion_aliases,
                                           deletion_aliases) {
  rule_values <- as.character(rules[[rule_var]])
  if (any(is.na(rule_values) | !nzchar(trimws(rule_values)))) {
    stop("`stanford_rules` contains missing or empty rule values.", call. = FALSE)
  }
  rules$.hiv_res_rule_id <- seq_len(nrow(rules))
  expanded <- vector("list", nrow(rules))
  for (i in seq_len(nrow(rules))) {
    rule_text <- trimws(rule_values[[i]])
    if (startsWith(rule_text, "+") || endsWith(rule_text, "+") || grepl("++", rule_text, fixed = TRUE)) {
      stop("`stanford_rules` contains an empty combination component.", call. = FALSE)
    }
    components <- strsplit(rule_text, "+", fixed = TRUE)[[1]]
    components <- trimws(components)
    if (length(components) == 0L || any(!nzchar(components))) {
      stop("`stanford_rules` contains an empty combination component.", call. = FALSE)
    }
    piece <- rules[rep(i, length(components)), , drop = FALSE]
    row.names(piece) <- NULL
    piece$.hiv_res_component_index <- seq_along(components)
    piece$.hiv_res_component_count <- length(components)
    piece$.hiv_res_component_text <- components
    expanded[[i]] <- piece
  }
  components <- do.call(rbind, expanded)
  parsed <- parse_hiv_resistance_mutations(
    dataset = components,
    result_var = .hiv_res_component_text,
    source_id_var = ".hiv_res_component_source_row",
    insertion_aliases = insertion_aliases,
    deletion_aliases = deletion_aliases,
    missing_values = NULL,
    keep_missing = FALSE,
    split_compact_substitutions = TRUE
  )
  parsed$.hiv_res_drug <- as.character(parsed[[drug_var]])
  parsed$.hiv_res_score <- parsed[[score_var]]
  parsed
}

.hiv_res_validate_gss_rules <- function(rule_components, join_keys) {
  if (nrow(rule_components) == 0L) {
    stop("`stanford_rules` produced no parseable mutation rules.", call. = FALSE)
  }
  single <- rule_components[rule_components$.hiv_res_component_count == 1L, , drop = FALSE]
  if (nrow(single) > 0L) {
    .hiv_res_error_on_ambiguous_reference(
      single,
      key_cols = unique(c(join_keys, ".hiv_res_drug")),
      value_cols = ".hiv_res_score",
      context = "`stanford_rules` single-mutation rules"
    )
  }
  combo_key <- c(".hiv_res_rule_id", ".hiv_res_component_index", join_keys)
  combo_dups <- duplicated(rule_components[combo_key])
  if (any(combo_dups)) {
    stop(
      "`stanford_rules` contains duplicate candidate mutations within the same combination component.",
      call. = FALSE
    )
  }
  .hiv_res_error_on_duplicate_combo_rules(rule_components, join_keys)
  rule_components
}

.hiv_res_error_on_duplicate_combo_rules <- function(rule_components, join_keys) {
  combo <- rule_components[rule_components$.hiv_res_component_count > 1L, , drop = FALSE]
  if (nrow(combo) == 0L) {
    return(invisible(NULL))
  }
  rule_ids <- unique(combo$.hiv_res_rule_id)
  combo_defs <- vector("list", length(rule_ids))
  for (i in seq_along(rule_ids)) {
    rule_id <- rule_ids[[i]]
    rule_rows <- combo[combo$.hiv_res_rule_id == rule_id, , drop = FALSE]
    component_ids <- unique(rule_rows$.hiv_res_component_index)
    component_keys <- character(length(component_ids))
    for (j in seq_along(component_ids)) {
      component_rows <- rule_rows[rule_rows$.hiv_res_component_index == component_ids[[j]], , drop = FALSE]
      candidate_keys <- sort(unique(.hiv_res_key(component_rows, join_keys)))
      component_keys[[j]] <- paste(candidate_keys, collapse = ",")
    }
    if (any(duplicated(component_keys))) {
      stop("`stanford_rules` contains repeated components within a combination rule.", call. = FALSE)
    }
    if (length(component_keys) > 1L) {
      component_sets <- strsplit(component_keys, ",", fixed = TRUE)
      for (left in seq_len(length(component_sets) - 1L)) {
        for (right in seq.int(left + 1L, length(component_sets))) {
          if (length(intersect(component_sets[[left]], component_sets[[right]])) > 0L) {
            stop("`stanford_rules` contains overlapping components within a combination rule.", call. = FALSE)
          }
        }
      }
    }
    combo_defs[[i]] <- data.frame(
      .hiv_res_drug = rule_rows$.hiv_res_drug[[1]],
      .hiv_res_combo_key = paste(sort(component_keys), collapse = "+"),
      .hiv_res_score = rule_rows$.hiv_res_score[[1]],
      stringsAsFactors = FALSE
    )
  }
  combo_defs <- do.call(rbind, combo_defs)
  duplicate_key <- duplicated(combo_defs[c(".hiv_res_drug", ".hiv_res_combo_key")]) |
    duplicated(combo_defs[c(".hiv_res_drug", ".hiv_res_combo_key")], fromLast = TRUE)
  if (any(duplicate_key)) {
    stop("`stanford_rules` contains duplicate combination reference rules.", call. = FALSE)
  }
  invisible(NULL)
}

.hiv_res_observed_scoring_mutations <- function(obs,
                                                key_vars,
                                                join_keys,
                                                duplicate_mutation_handling) {
  keep_cols <- unique(c(key_vars, join_keys, "REFRES", "GENLOC", "CODON", "AAS", "RESCAT"))
  keep_cols <- keep_cols[keep_cols %in% names(obs)]
  obs_unique <- obs[, keep_cols, drop = FALSE]
  dup_cols <- unique(c(key_vars, join_keys))
  if (nrow(obs_unique) > 0L) {
    duplicated_rows <- duplicated(obs_unique[dup_cols]) | duplicated(obs_unique[dup_cols], fromLast = TRUE)
    if (any(duplicated_rows)) {
      if (duplicate_mutation_handling == "error") {
        stop(
          "Duplicate parsed mutations were found within `key_vars`; set ",
          "`duplicate_mutation_handling = \"distinct\"` to collapse exact duplicates.",
          call. = FALSE
        )
      }
      obs_unique <- unique(obs_unique)
    }
  }
  obs_unique
}

.hiv_res_score_stanford_single <- function(obs_unique, rule_components, join_keys, key_vars) {
  empty <- .hiv_res_empty_score(key_vars)
  single <- rule_components[rule_components$.hiv_res_component_count == 1L, , drop = FALSE]
  if (nrow(obs_unique) == 0L || nrow(single) == 0L) {
    return(empty)
  }
  single_lookup <- unique(single[, unique(c(join_keys, ".hiv_res_drug", ".hiv_res_score")), drop = FALSE])
  matched <- merge(obs_unique, single_lookup, by = join_keys, all = FALSE, sort = FALSE)
  if (nrow(matched) == 0L) {
    return(empty)
  }
  matched$.hiv_res_applied_score <- matched$.hiv_res_score
  wild_type <- !is.na(matched$.hiv_res_applied_score) &
    matched$.hiv_res_applied_score <= 0 &
    !is.na(matched$REFRES) &
    !is.na(matched$AAS) &
    matched$REFRES == matched$AAS
  matched$.hiv_res_applied_score[wild_type] <- 0

  max_cols <- unique(c(key_vars, ".hiv_res_drug", intersect("GENROICD", names(matched)), "CODON"))
  per_codon <- stats::aggregate(
    matched[".hiv_res_applied_score"],
    matched[max_cols],
    max,
    na.rm = FALSE
  )
  sum_cols <- unique(c(key_vars, ".hiv_res_drug"))
  stats::aggregate(
    per_codon[".hiv_res_applied_score"],
    per_codon[sum_cols],
    sum,
    na.rm = FALSE
  )
}

.hiv_res_score_stanford_combo <- function(obs_unique, rule_components, join_keys, key_vars) {
  empty <- .hiv_res_empty_score(key_vars)
  combo <- rule_components[rule_components$.hiv_res_component_count > 1L, , drop = FALSE]
  if (nrow(obs_unique) == 0L || nrow(combo) == 0L) {
    return(empty)
  }
  combo_lookup <- unique(combo[, unique(c(
    join_keys,
    ".hiv_res_rule_id",
    ".hiv_res_component_index",
    ".hiv_res_component_count",
    ".hiv_res_drug",
    ".hiv_res_score"
  )), drop = FALSE])
  matched <- merge(obs_unique, combo_lookup, by = join_keys, all = FALSE, sort = FALSE)
  if (nrow(matched) == 0L) {
    return(empty)
  }
  component_cols <- unique(c(
    key_vars,
    ".hiv_res_drug",
    ".hiv_res_rule_id",
    ".hiv_res_component_index",
    ".hiv_res_component_count",
    ".hiv_res_score"
  ))
  matched_components <- unique(matched[, component_cols, drop = FALSE])
  rule_cols <- unique(c(key_vars, ".hiv_res_drug", ".hiv_res_rule_id", ".hiv_res_component_count", ".hiv_res_score"))
  component_counts <- stats::aggregate(
    matched_components[".hiv_res_component_index"],
    matched_components[rule_cols],
    function(x) length(unique(x))
  )
  names(component_counts)[names(component_counts) == ".hiv_res_component_index"] <- ".hiv_res_matched_components"
  complete <- component_counts$.hiv_res_matched_components == component_counts$.hiv_res_component_count
  complete_rules <- component_counts[complete, , drop = FALSE]
  if (nrow(complete_rules) == 0L) {
    return(empty)
  }
  sum_cols <- unique(c(key_vars, ".hiv_res_drug"))
  stats::aggregate(
    complete_rules[".hiv_res_score"],
    complete_rules[sum_cols],
    sum,
    na.rm = FALSE
  )
}

.hiv_res_empty_score <- function(key_vars) {
  out <- data.frame(stringsAsFactors = FALSE)
  for (key_var in key_vars) {
    out[[key_var]] <- character(0)
  }
  out$.hiv_res_drug <- character(0)
  out$.hiv_res_applied_score <- numeric(0)
  out
}

.hiv_res_gss_drug_reference <- function(rules,
                                        drug_reference,
                                        drug_var,
                                        drug_name_var,
                                        rule_class_var,
                                        rule_region_code_var,
                                        rule_region_name_var) {
  if (!is.null(drug_reference)) {
    if (!inherits(drug_reference, "data.frame")) {
      stop("`drug_reference` must be a data frame or tibble.", call. = FALSE)
    }
    .hiv_res_require_columns(drug_reference, drug_var, "`drug_reference`")
    drug_ref <- as.data.frame(drug_reference, stringsAsFactors = FALSE)
  } else {
    ref_cols <- c(drug_var, drug_name_var, rule_class_var, rule_region_code_var, rule_region_name_var)
    ref_cols <- unique(ref_cols[!is.null(ref_cols) & ref_cols %in% names(rules)])
    drug_ref <- unique(rules[, ref_cols, drop = FALSE])
  }
  if (nrow(drug_ref) == 0L) {
    stop("`drug_reference`/`stanford_rules` must define at least one drug.", call. = FALSE)
  }
  if (any(is.na(drug_ref[[drug_var]]) | !nzchar(as.character(drug_ref[[drug_var]])))) {
    stop("Drug reference contains missing or empty drug codes.", call. = FALSE)
  }
  if (any(duplicated(as.character(drug_ref[[drug_var]])))) {
    stop("Drug reference must contain no duplicate drug codes.", call. = FALSE)
  }
  drug_ref
}

.hiv_res_build_gss_output <- function(dataset,
                                      key_vars,
                                      drug_ref,
                                      drug_var,
                                      single_scores,
                                      combo_scores,
                                      penalty_score_var,
                                      single_score_var,
                                      combination_score_var) {
  key_frame <- unique(dataset[, key_vars, drop = FALSE])
  key_frame$.hiv_res_key_order <- seq_len(nrow(key_frame))
  drug_ref$.hiv_res_drug_order <- seq_len(nrow(drug_ref))
  output <- merge(key_frame, drug_ref, by = NULL, sort = FALSE)

  single <- single_scores
  if (nrow(single) > 0L) {
    names(single)[names(single) == ".hiv_res_drug"] <- drug_var
    names(single)[names(single) == ".hiv_res_applied_score"] <- single_score_var
    output <- merge(output, single, by = c(key_vars, drug_var), all.x = TRUE, sort = FALSE)
  } else {
    output[[single_score_var]] <- NA_real_
  }

  combo <- combo_scores
  if (nrow(combo) > 0L) {
    names(combo)[names(combo) == ".hiv_res_drug"] <- drug_var
    names(combo)[names(combo) == ".hiv_res_score"] <- combination_score_var
    output <- merge(output, combo, by = c(key_vars, drug_var), all.x = TRUE, sort = FALSE)
  } else {
    output[[combination_score_var]] <- NA_real_
  }

  output[[single_score_var]][is.na(output[[single_score_var]])] <- 0
  output[[combination_score_var]][is.na(output[[combination_score_var]])] <- 0
  output[[penalty_score_var]] <- output[[single_score_var]] + output[[combination_score_var]]
  output[[penalty_score_var]] <- ifelse(output[[penalty_score_var]] < 0, 0, output[[penalty_score_var]])
  output <- output[order(output$.hiv_res_key_order, output$.hiv_res_drug_order), , drop = FALSE]
  output$.hiv_res_key_order <- NULL
  output$.hiv_res_drug_order <- NULL
  row.names(output) <- NULL
  output
}

.hiv_res_add_regimen_total <- function(result,
                                       key_vars,
                                       drug_var,
                                       drug_name_var,
                                       regimen_drugs,
                                       total_drug_code,
                                       total_drug_name,
                                       penalty_score_var,
                                       single_score_var,
                                       combination_score_var) {
  .hiv_res_check_character_vector(regimen_drugs, "regimen_drugs", allow_null = FALSE)
  if (any(duplicated(regimen_drugs))) {
    stop("`regimen_drugs` must not contain duplicates.", call. = FALSE)
  }
  missing_drugs <- setdiff(regimen_drugs, as.character(result[[drug_var]]))
  if (length(missing_drugs) > 0L) {
    stop(
      "`regimen_drugs` contains drug code(s) not present in the emitted drug reference: ",
      paste(missing_drugs, collapse = ", "),
      call. = FALSE
    )
  }
  regimen_rows <- result[as.character(result[[drug_var]]) %in% regimen_drugs, , drop = FALSE]
  score_cols <- c(penalty_score_var, single_score_var, combination_score_var)
  total <- stats::aggregate(regimen_rows[score_cols], regimen_rows[key_vars], sum, na.rm = FALSE)
  for (col in setdiff(names(result), c(key_vars, score_cols))) {
    total[[col]] <- NA
  }
  total[[drug_var]] <- total_drug_code
  if (!is.null(drug_name_var) && drug_name_var %in% names(total)) {
    total[[drug_name_var]] <- total_drug_name
  }
  total <- total[, names(result), drop = FALSE]
  rbind(result, total)
}

.hiv_res_add_class_region <- function(data,
                                      region_code_var,
                                      region_name_var,
                                      class_var,
                                      class_region_map,
                                      class_region_map_class_var,
                                      class_region_map_code_var,
                                      class_region_map_name_var,
                                      caller) {
  if (region_code_var %in% names(data)) {
    return(data)
  }
  if (is.null(class_region_map)) {
    return(data)
  }
  .hiv_res_require_columns(data, class_var, caller)
  if (!inherits(class_region_map, "data.frame")) {
    stop("`class_region_map` must be a data frame or tibble.", call. = FALSE)
  }
  .hiv_res_require_columns(
    class_region_map,
    c(class_region_map_class_var, class_region_map_code_var, class_region_map_name_var),
    "`class_region_map`"
  )
  map <- as.data.frame(class_region_map, stringsAsFactors = FALSE)
  map_key <- toupper(as.character(map[[class_region_map_class_var]]))
  if (any(is.na(map_key) | !nzchar(map_key))) {
    stop("`class_region_map` contains missing or empty class values.", call. = FALSE)
  }
  if (any(duplicated(map_key))) {
    stop("`class_region_map` contains duplicate class values.", call. = FALSE)
  }
  data_key <- toupper(as.character(data[[class_var]]))
  idx <- match(data_key, map_key)
  unmatched <- is.na(idx) & !is.na(data_key) & nzchar(data_key)
  if (any(unmatched)) {
    stop(
      caller,
      " contains class value(s) not present in `class_region_map`: ",
      paste(unique(as.character(data[[class_var]][unmatched])), collapse = ", "),
      call. = FALSE
    )
  }
  data[[region_code_var]] <- map[[class_region_map_code_var]][idx]
  data[[region_name_var]] <- map[[class_region_map_name_var]][idx]
  data
}

.hiv_res_map_region <- function(values,
                                region_map,
                                region_map_region_var,
                                region_map_code_var,
                                region_map_name_var,
                                region_map_match) {
  if (!inherits(region_map, "data.frame")) {
    stop("`region_map` must be a data frame or tibble.", call. = FALSE)
  }
  .hiv_res_require_columns(
    region_map,
    c(region_map_region_var, region_map_code_var, region_map_name_var),
    "`region_map`"
  )
  map <- as.data.frame(region_map, stringsAsFactors = FALSE)
  map_values <- as.character(map[[region_map_region_var]])
  if (any(is.na(map_values) | !nzchar(map_values))) {
    stop("`region_map` contains missing or empty region values.", call. = FALSE)
  }
  if (region_map_match == "value" && any(duplicated(toupper(map_values)))) {
    stop("`region_map` contains duplicate region values.", call. = FALSE)
  }
  code <- rep(NA_character_, length(values))
  name <- rep(NA_character_, length(values))
  for (i in seq_along(values)) {
    value <- as.character(values[[i]])
    if (is.na(value) || !nzchar(value)) {
      next
    }
    if (region_map_match == "value") {
      idx <- which(toupper(map_values) == toupper(value))
    } else {
      idx <- which(startsWith(toupper(value), toupper(map_values)))
    }
    if (length(idx) == 0L) {
      stop("`region_map` has no match for region value \"", value, "\".", call. = FALSE)
    }
    if (length(idx) > 1L) {
      stop("`region_map` matches region value \"", value, "\" ambiguously.", call. = FALSE)
    }
    code[[i]] <- as.character(map[[region_map_code_var]][[idx]])
    name[[i]] <- as.character(map[[region_map_name_var]][[idx]])
  }
  data.frame(code = code, name = name, stringsAsFactors = FALSE)
}

.hiv_res_align_region_join <- function(observed,
                                       reference,
                                       observed_region_code_var,
                                       reference_region_code_var,
                                       allow_region_agnostic_matching,
                                       context) {
  keys <- c("CODON", "AAS", "RESCAT")
  if (nrow(observed) == 0L) {
    return(list(observed = observed, reference = reference, join_keys = keys))
  }

  observed_has_region <- .hiv_res_column_has_values(observed, observed_region_code_var)
  reference_has_region <- .hiv_res_column_has_values(reference, reference_region_code_var)

  if (observed_has_region && reference_has_region) {
    if (.hiv_res_column_has_missing_values(reference, reference_region_code_var)) {
      stop(
        context,
        " contains mixed missing and populated region codes; supply complete region codes ",
        "or pre-process to a region-agnostic reference before scoring.",
        call. = FALSE
      )
    }
    if (.hiv_res_column_has_missing_values(observed, observed_region_code_var)) {
      stop(
        "Observed mutations contain missing mapped region codes while ",
        context,
        " is region-coded.",
        call. = FALSE
      )
    }
    if (observed_region_code_var != reference_region_code_var) {
      if (observed_region_code_var %in% names(reference)) {
        stop(context, " has conflicting region-code columns for matching.", call. = FALSE)
      }
      names(reference)[names(reference) == reference_region_code_var] <- observed_region_code_var
    }
    return(list(
      observed = observed,
      reference = reference,
      join_keys = c(observed_region_code_var, keys)
    ))
  }

  if (!allow_region_agnostic_matching && observed_has_region != reference_has_region) {
    stop(
      context,
      " and observed mutations must either both contain mapped region codes or ",
      "`allow_region_agnostic_matching` must be set to TRUE after caller validation.",
      call. = FALSE
    )
  }

  list(observed = observed, reference = reference, join_keys = keys)
}

.hiv_res_column_has_values <- function(data, column) {
  if (is.null(column) || !column %in% names(data)) {
    return(FALSE)
  }
  values <- as.character(data[[column]])
  any(!is.na(values) & nzchar(values))
}

.hiv_res_column_has_missing_values <- function(data, column) {
  if (is.null(column) || !column %in% names(data) || nrow(data) == 0L) {
    return(FALSE)
  }
  values <- as.character(data[[column]])
  any(is.na(values) | !nzchar(values))
}

.hiv_res_error_on_ambiguous_reference <- function(data, key_cols, value_cols, context) {
  if (nrow(data) == 0L || length(value_cols) == 0L) {
    return(invisible(NULL))
  }
  keys <- .hiv_res_key(data, key_cols)
  duplicate_keys <- unique(keys[duplicated(keys)])
  if (length(duplicate_keys) == 0L) {
    return(invisible(NULL))
  }
  for (key in duplicate_keys) {
    idx <- which(keys == key)
    values <- unique(data[idx, value_cols, drop = FALSE])
    if (nrow(values) > 1L) {
      stop(context, " contains ambiguous duplicate reference rules.", call. = FALSE)
    }
  }
  invisible(NULL)
}

.hiv_res_map_mcrit_class <- function(values,
                                     mcrit_class_map,
                                     mcrit_class_map_class_var,
                                     mcrit_class_map_label_var) {
  if (is.null(mcrit_class_map)) {
    return(as.character(values))
  }
  if (!inherits(mcrit_class_map, "data.frame")) {
    stop("`mcrit_class_map` must be a data frame or tibble.", call. = FALSE)
  }
  .hiv_res_require_columns(
    mcrit_class_map,
    c(mcrit_class_map_class_var, mcrit_class_map_label_var),
    "`mcrit_class_map`"
  )
  map_key <- toupper(as.character(mcrit_class_map[[mcrit_class_map_class_var]]))
  if (any(duplicated(map_key))) {
    stop("`mcrit_class_map` contains duplicate class values.", call. = FALSE)
  }
  idx <- match(toupper(as.character(values)), map_key)
  unmatched <- is.na(idx) & !is.na(values) & nzchar(as.character(values))
  if (any(unmatched)) {
    stop(
      "`mcrit_class_map` has no match for class value(s): ",
      paste(unique(as.character(values[unmatched])), collapse = ", "),
      call. = FALSE
    )
  }
  as.character(mcrit_class_map[[mcrit_class_map_label_var]][idx])
}

.hiv_res_match_insertion_position_rule <- function(value, dictionary_insertion_position_regex) {
  if (is.null(dictionary_insertion_position_regex)) {
    return(NA_character_)
  }

  matches <- character(0)
  for (pattern in dictionary_insertion_position_regex) {
    pieces <- tryCatch(
      {
        match <- regexec(pattern, value, perl = TRUE)
        regmatches(value, match)[[1]]
      },
      error = function(e) {
        stop(
          "`dictionary_insertion_position_regex` contains an invalid regular expression: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
    if (length(pieces) > 0L) {
      if (length(pieces) < 2L) {
        stop(
          "`dictionary_insertion_position_regex` must include a first capture group for the codon number.",
          call. = FALSE
        )
      }
      matches <- c(matches, pieces[[2]])
    }
  }

  if (length(matches) == 0L) {
    return(NA_character_)
  }
  if (length(matches) > 1L) {
    stop(
      "`dictionary_insertion_position_regex` matches IAS rule value \"",
      value,
      "\" ambiguously.",
      call. = FALSE
    )
  }
  if (!grepl("^[0-9]+$", matches[[1]])) {
    stop(
      "`dictionary_insertion_position_regex` captured a non-numeric codon number for IAS rule value \"",
      value,
      "\".",
      call. = FALSE
    )
  }
  matches[[1]]
}

.hiv_res_format_excluded_rows <- function(excluded) {
  paste0(
    "row ",
    excluded$row,
    " genotype ",
    vapply(excluded$genotype, .hiv_res_quote_value, character(1)),
    " (",
    excluded$reason,
    ")"
  )
}

.hiv_res_quote_value <- function(value) {
  if (is.na(value)) {
    return("<NA>")
  }
  paste0("\"", value, "\"")
}

.hiv_res_alias_category <- function(token, insertion_aliases, deletion_aliases, case_sensitive) {
  norm <- .hiv_res_normalize_value(token, case_sensitive)
  ins <- .hiv_res_normalize_values(insertion_aliases, case_sensitive)
  del <- .hiv_res_normalize_values(deletion_aliases, case_sensitive)
  if (norm %in% ins) {
    return("INSERTION")
  }
  if (norm %in% del) {
    return("DELETION")
  }
  NA_character_
}

.hiv_res_extract_alias <- function(token, insertion_aliases, deletion_aliases, case_sensitive) {
  norm <- .hiv_res_normalize_value(token, case_sensitive)
  ins <- .hiv_res_normalize_values(insertion_aliases, case_sensitive)
  del <- .hiv_res_normalize_values(deletion_aliases, case_sensitive)
  aliases <- data.frame(
    alias = c(ins, del),
    category = c(rep("INSERTION", length(ins)), rep("DELETION", length(del))),
    stringsAsFactors = FALSE
  )
  aliases <- aliases[order(nchar(aliases$alias), decreasing = TRUE), , drop = FALSE]
  for (i in seq_len(nrow(aliases))) {
    alias <- aliases$alias[[i]]
    if (!nzchar(alias)) {
      next
    }
    if (identical(norm, alias)) {
      return(list(category = aliases$category[[i]], remainder = ""))
    }
    if (startsWith(norm, alias)) {
      return(list(category = aliases$category[[i]], remainder = substr(norm, nchar(alias) + 1L, nchar(norm))))
    }
  }
  list(category = NA_character_, remainder = norm)
}

.hiv_res_normalize_value <- function(value, case_sensitive) {
  out <- trimws(as.character(value))
  if (!case_sensitive) {
    out <- toupper(out)
  }
  out
}

.hiv_res_normalize_values <- function(values, case_sensitive) {
  if (is.null(values)) {
    return(NULL)
  }
  values <- trimws(as.character(values))
  if (!case_sensitive) {
    values <- toupper(values)
  }
  values
}

.hiv_res_key <- function(data, cols) {
  if (length(cols) == 0L) {
    return(rep(".__all__", nrow(data)))
  }
  parts <- lapply(cols, function(col) {
    value <- as.character(data[[col]])
    value[is.na(value)] <- "<NA>"
    value
  })
  do.call(paste, c(parts, sep = "\r"))
}

.hiv_res_require_columns <- function(data, columns, arg) {
  columns <- unique(columns[!is.null(columns) & !is.na(columns) & nzchar(columns)])
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    stop(
      arg,
      " is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

.hiv_res_check_output_columns <- function(data, columns, overwrite) {
  columns <- columns[!is.na(columns) & nzchar(columns)]
  if (any(duplicated(columns))) {
    stop("Parser output column names must be unique.", call. = FALSE)
  }
  columns <- unique(columns)
  if (!overwrite) {
    existing <- intersect(columns, names(data))
    if (length(existing) > 0L) {
      stop(
        "Output column(s) already exist in `dataset`: ",
        paste(existing, collapse = ", "),
        ". Set `overwrite = TRUE` to replace them.",
        call. = FALSE
      )
    }
  }
}

.hiv_res_check_aliases <- function(aliases, arg) {
  .hiv_res_check_character_vector(aliases, arg, allow_null = FALSE)
  if (any(!nzchar(trimws(aliases)))) {
    stop("`", arg, "` must not contain empty aliases.", call. = FALSE)
  }
}

.hiv_res_check_alias_sets <- function(insertion_aliases, deletion_aliases, case_sensitive) {
  insertion <- .hiv_res_normalize_values(insertion_aliases, case_sensitive)
  deletion <- .hiv_res_normalize_values(deletion_aliases, case_sensitive)
  if (any(duplicated(insertion))) {
    stop("`insertion_aliases` must be unique after case normalization.", call. = FALSE)
  }
  if (any(duplicated(deletion))) {
    stop("`deletion_aliases` must be unique after case normalization.", call. = FALSE)
  }
  overlap <- intersect(insertion, deletion)
  if (length(overlap) > 0L) {
    stop(
      "`insertion_aliases` and `deletion_aliases` overlap after case normalization: ",
      paste(overlap, collapse = ", "),
      call. = FALSE
    )
  }
}

.hiv_res_check_structural_regex <- function(dictionary_insertion_position_regex) {
  if (is.null(dictionary_insertion_position_regex)) {
    return(invisible(NULL))
  }
  .hiv_res_check_character_vector(
    dictionary_insertion_position_regex,
    "dictionary_insertion_position_regex",
    allow_null = FALSE
  )
  for (pattern in dictionary_insertion_position_regex) {
    tryCatch(
      regexec(pattern, "", perl = TRUE),
      error = function(e) {
        stop(
          "`dictionary_insertion_position_regex` contains an invalid regular expression: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }
  invisible(NULL)
}

.hiv_res_check_unparseable_rows <- function(unparseable_dictionary_rows,
                                            n_rules,
                                            unparseable_dictionary) {
  if (is.null(unparseable_dictionary_rows)) {
    return(invisible(NULL))
  }
  if (unparseable_dictionary != "exclude") {
    stop(
      "`unparseable_dictionary_rows` can only be supplied when ",
      "`unparseable_dictionary = \"exclude\"`.",
      call. = FALSE
    )
  }
  if (!is.numeric(unparseable_dictionary_rows) ||
      length(unparseable_dictionary_rows) == 0L ||
      any(is.na(unparseable_dictionary_rows)) ||
      any(unparseable_dictionary_rows != as.integer(unparseable_dictionary_rows))) {
    stop("`unparseable_dictionary_rows` must be a non-empty integer vector.", call. = FALSE)
  }
  if (any(unparseable_dictionary_rows < 1L | unparseable_dictionary_rows > n_rules)) {
    stop(
      "`unparseable_dictionary_rows` contains row number(s) outside the IAS rule table.",
      call. = FALSE
    )
  }
  if (any(duplicated(unparseable_dictionary_rows))) {
    stop("`unparseable_dictionary_rows` must not contain duplicate row numbers.", call. = FALSE)
  }
  invisible(NULL)
}

.hiv_res_check_scalar_name <- function(value, arg) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop("`", arg, "` must be a single non-empty character value.", call. = FALSE)
  }
}

.hiv_res_check_character_vector <- function(value, arg, allow_null = TRUE) {
  if (is.null(value)) {
    if (allow_null) {
      return(invisible(NULL))
    }
    stop("`", arg, "` must be supplied.", call. = FALSE)
  }
  if (!is.character(value) || length(value) == 0L || any(is.na(value))) {
    stop("`", arg, "` must be a non-empty character vector without missing values.", call. = FALSE)
  }
  if (any(!nzchar(value))) {
    stop("`", arg, "` must not contain empty values.", call. = FALSE)
  }
  invisible(NULL)
}

.hiv_res_column_name <- function(expr, env, arg) {
  if (rlang::is_string(expr)) {
    return(as.character(expr))
  }
  if (rlang::is_symbol(expr)) {
    name <- rlang::as_name(expr)
    if (exists(name, envir = env, inherits = TRUE)) {
      evaluated <- get(name, envir = env, inherits = TRUE)
      if (is.character(evaluated) && length(evaluated) == 1L &&
          !is.na(evaluated) && nzchar(evaluated)) {
        return(evaluated)
      }
    }
    return(name)
  }
  evaluated <- eval(expr, envir = env)
  if (is.character(evaluated) && length(evaluated) == 1L &&
      !is.na(evaluated) && nzchar(evaluated)) {
    return(evaluated)
  }
  stop("`", arg, "` must be a column name supplied bare or as a string.", call. = FALSE)
}

.hiv_res_check_logical_scalar <- function(value, arg) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop("`", arg, "` must be a single non-missing logical value.", call. = FALSE)
  }
  invisible(NULL)
}

.hiv_res_check_reference_version <- function(reference_version) {
  if (is.null(reference_version)) {
    return(invisible(NULL))
  }
  if (!is.character(reference_version) || length(reference_version) != 1L ||
      is.na(reference_version) || !nzchar(reference_version)) {
    stop("`reference_version` must be a single non-empty character value when supplied.", call. = FALSE)
  }
  invisible(NULL)
}
