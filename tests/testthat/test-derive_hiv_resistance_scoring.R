test_that("parse_hiv_resistance_mutations expands alternatives and indels", {
  dat <- data.frame(
    USUBJID = "SYN-001",
    GFSTRESC = c("K103N", "T66A/I/K", "INS69S_ABC", "T69DEL"),
    GFGENRI = c("RT", "IN", "IN", "IN"),
    stringsAsFactors = FALSE
  )
  region_map <- data.frame(
    REGION = c("RT", "IN"),
    GENROICD = c("RT", "IN"),
    GENROI = c("REVERSE TRANSCRIPTASE", "INTEGRASE"),
    stringsAsFactors = FALSE
  )

  result <- parse_hiv_resistance_mutations(
    dat,
    result_var = "GFSTRESC",
    region_var = "GFGENRI",
    region_map = region_map
  )

  expect_equal(nrow(result), 6)
  expect_equal(result$.hiv_res_source_row, c(1, 2, 2, 2, 3, 4))
  expect_equal(result$REFRES, c("K", "T", "T", "T", "INS", "T"))
  expect_equal(result$GENLOC, c("103", "66", "66", "66", "69", "69"))
  expect_equal(result$AAS, c("N", "A", "I", "K", "S", NA))
  expect_equal(result$AASSEQ, c(NA, NA, NA, NA, "ABC", NA))
  expect_equal(result$MUTATION, c("K103N", "T66A", "T66I", "T66K", "INS69S", "T69DEL"))
  expect_equal(result$RESCAT, c(NA, NA, NA, NA, "INSERTION", "DELETION"))
  expect_equal(result$GENROICD, c("RT", "IN", "IN", "IN", "IN", "IN"))

  expect_error(
    parse_hiv_resistance_mutations(data.frame(GFSTRESC = "not_a_mutation"), "GFSTRESC"),
    "Malformed HIV resistance mutation syntax"
  )
  expect_error(
    parse_hiv_resistance_mutations(data.frame(GFSTRESC = "K103N+"), "GFSTRESC"),
    "Malformed HIV resistance mutation syntax"
  )
  expect_error(
    parse_hiv_resistance_mutations(data.frame(GFSTRESC = "INS69S/"), "GFSTRESC"),
    "Malformed HIV resistance mutation syntax"
  )
  expect_error(
    parse_hiv_resistance_mutations(
      data.frame(GFSTRESC = "INS69"),
      "GFSTRESC",
      insertion_aliases = "INS",
      deletion_aliases = "INS"
    ),
    "overlap"
  )
})

test_that("derive_hiv_ias_resistance classifies IAS and caller prespecified rules only", {
  dat <- data.frame(
    USUBJID = "SYN-001",
    VISITNUM = 1,
    GFSTRESC = c("K103N", "T66A/I/K", "L10I"),
    GFGENRI = c("RT", "IN", "PR"),
    stringsAsFactors = FALSE
  )
  region_map <- data.frame(
    REGION = c("RT", "IN", "PR"),
    GENROICD = c("RT", "IN", "PR"),
    GENROI = c("REVERSE TRANSCRIPTASE", "INTEGRASE", "PROTEASE"),
    stringsAsFactors = FALSE
  )
  ias_dictionary <- data.frame(
    GENOTYPE = c("K103N", "M184V"),
    GENROICD = c("RT", "RT"),
    CLASS = c("NNRTI", "NRTI"),
    SUBCLASS = c("MAJOR", "MAJOR"),
    stringsAsFactors = FALSE
  )
  prespecified <- data.frame(
    GENOTYPE = "T66A/I",
    GENROICD = "IN",
    CLASS = "INSTI",
    stringsAsFactors = FALSE
  )

  result <- derive_hiv_ias_resistance(
    dat,
    ias_dictionary,
    result_var = "GFSTRESC",
    key_vars = c("USUBJID", "VISITNUM"),
    region_var = "GFGENRI",
    region_map = region_map,
    prespecified_rules = prespecified,
    reference_version = "US-IAS-SYN"
  )

  matched <- result[!is.na(result$MATCHFL), ]
  expect_equal(matched$MUTATION, c("K103N", "T66A", "T66I"))
  expect_equal(result$MCRIT2[result$MUTATION == "K103N"], "Major Mutation")
  expect_equal(result$MCRIT2[result$MUTATION == "T66A"], "Specified Mutation")
  expect_true(is.na(result$MATCHFL[result$MUTATION == "T66K"]))
  expect_true(is.na(result$MATCHFL[result$MUTATION == "L10I"]))
  expect_true(all(result$REFVER == "US-IAS-SYN"))

  multi_drug_dictionary <- data.frame(
    GENOTYPE = c("K103N", "K103N"),
    GENROICD = c("RT", "RT"),
    CLASS = c("NNRTI", "NNRTI"),
    SUBCLASS = c("Major", "Major"),
    DRUGCODE = c("EFV", "NVP"),
    DRUGNAME = c("Efavirenz", "Nevirapine"),
    stringsAsFactors = FALSE
  )
  multi_drug_result <- derive_hiv_ias_resistance(
    dat[1, ],
    multi_drug_dictionary,
    result_var = "GFSTRESC",
    key_vars = c("USUBJID", "VISITNUM"),
    region_var = "GFGENRI",
    region_map = region_map,
    dictionary_drug_code_var = "DRUGCODE",
    dictionary_drug_name_var = "DRUGNAME"
  )
  expect_equal(nrow(multi_drug_result), 2)
  expect_setequal(multi_drug_result$IAS_DRUG_CODE, c("EFV", "NVP"))

  ambiguous_dictionary <- rbind(
    ias_dictionary[1, ],
    data.frame(GENOTYPE = "K103N", GENROICD = "RT", CLASS = "NRTI", SUBCLASS = "MAJOR")
  )
  expect_error(
    derive_hiv_ias_resistance(
      dat[1, ],
      ambiguous_dictionary,
      result_var = "GFSTRESC",
      region_var = "GFGENRI",
      region_map = region_map
    ),
    "ambiguous duplicate reference rules"
  )
})

test_that("derive_hiv_ias_resistance supports explicit IAS insertion-position rules only", {
  dat <- data.frame(
    USUBJID = "SYN-INS",
    VISITNUM = 1,
    GFSTRESC = c("INS69S_ABC", "T69DEL", "T69S"),
    GFGENRI = c("RT", "RT", "RT"),
    stringsAsFactors = FALSE
  )
  region_map <- data.frame(
    REGION = "RT",
    GENROICD = "RT",
    GENROI = "REVERSE TRANSCRIPTASE",
    stringsAsFactors = FALSE
  )
  class_region_map <- data.frame(
    CLASS = "NRTI",
    GENROICD = "RT",
    GENROI = "REVERSE TRANSCRIPTASE",
    stringsAsFactors = FALSE
  )
  ias_dictionary <- data.frame(
    CLASS = "NRTI",
    DRUGNAME = "69 Insertion Complex (all nRTIs)",
    DRUGCODE = "",
    GENOTYPE = "_69",
    SUBCLASS = "Major",
    VERSION = "2022",
    stringsAsFactors = FALSE
  )

  expect_error(
    derive_hiv_ias_resistance(
      dat,
      ias_dictionary,
      result_var = "GFSTRESC",
      key_vars = c("USUBJID", "VISITNUM"),
      region_var = "GFGENRI",
      region_map = region_map,
      class_region_map = class_region_map,
      dictionary_drug_code_var = "DRUGCODE",
      dictionary_drug_name_var = "DRUGNAME"
    ),
    "unparseable rule"
  )

  result <- derive_hiv_ias_resistance(
    dat,
    ias_dictionary,
    result_var = "GFSTRESC",
    key_vars = c("USUBJID", "VISITNUM"),
    region_var = "GFGENRI",
    region_map = region_map,
    class_region_map = class_region_map,
    dictionary_drug_code_var = "DRUGCODE",
    dictionary_drug_name_var = "DRUGNAME",
    dictionary_insertion_position_regex = "^_([0-9]+)$",
    reference_version = "IAS-SYN-2024"
  )

  expect_equal(result$MUTATION, c("INS69S", "T69DEL", "T69S"))
  expect_equal(result$MATCHFL, c("Y", NA, NA))
  matched_row <- which(result$MATCHFL == "Y")
  expect_equal(result$IAS_GENOTYPE[matched_row], "_69")
  expect_equal(result$IAS_DRUG_NAME[matched_row], "69 Insertion Complex (all nRTIs)")
  expect_true(all(is.na(result$MATCHFL[result$RESCAT == "DELETION"])))
  expect_true(is.na(result$MATCHFL[is.na(result$RESCAT) & result$GENLOC == "69"]))
  expect_true(all(result$REFVER == "IAS-SYN-2024"))
})

test_that("derive_hiv_ias_resistance requires auditable opt-in for unparseable dictionary rows", {
  dat <- data.frame(
    USUBJID = "SYN-BLANK",
    VISITNUM = 1,
    GFSTRESC = "K103N",
    stringsAsFactors = FALSE
  )
  blank_dictionary <- data.frame(
    GENOTYPE = c("K103N", ""),
    CLASS = c("NNRTI", "EI"),
    SUBCLASS = c("Major", "Major"),
    stringsAsFactors = FALSE
  )

  expect_error(
    derive_hiv_ias_resistance(
      dat,
      blank_dictionary,
      result_var = "GFSTRESC",
      key_vars = c("USUBJID", "VISITNUM")
    ),
    "unparseable rule"
  )

  expect_warning(
    result <- derive_hiv_ias_resistance(
      dat,
      blank_dictionary,
      result_var = "GFSTRESC",
      key_vars = c("USUBJID", "VISITNUM"),
      unparseable_dictionary = "exclude"
    ),
    "row 2 genotype \"\""
  )
  expect_equal(result$MATCHFL, "Y")

  malformed_dictionary <- data.frame(
    GENOTYPE = c("K103N", "BAD_RULE"),
    CLASS = c("NNRTI", "NNRTI"),
    SUBCLASS = c("Major", "Major"),
    stringsAsFactors = FALSE
  )
  expect_error(
    derive_hiv_ias_resistance(
      dat,
      malformed_dictionary,
      result_var = "GFSTRESC",
      key_vars = c("USUBJID", "VISITNUM"),
      unparseable_dictionary = "exclude"
    ),
    "not explicitly listed"
  )
  expect_warning(
    result_explicit <- derive_hiv_ias_resistance(
      dat,
      malformed_dictionary,
      result_var = "GFSTRESC",
      key_vars = c("USUBJID", "VISITNUM"),
      unparseable_dictionary = "exclude",
      unparseable_dictionary_rows = 2L
    ),
    "row 2 genotype \"BAD_RULE\""
  )
  expect_equal(result_explicit$MATCHFL, "Y")
})

test_that("derive_hiv_stanford_gss scores single, combination, zero, total, and categories", {
  dat <- data.frame(
    USUBJID = c("SYN-001", "SYN-001"),
    VISITNUM = c(1, 1),
    GFSTRESC = c("K103N/S", "Y181C"),
    GFGENRI = c("RT", "RT"),
    stringsAsFactors = FALSE
  )
  region_map <- data.frame(
    REGION = c("RT", "PR"),
    GENROICD = c("RT", "PR"),
    GENROI = c("REVERSE TRANSCRIPTASE", "PROTEASE"),
    stringsAsFactors = FALSE
  )
  stanford_rules <- data.frame(
    GENOTYPE = c("K103N", "K103S", "K103N+Y181C", "V11IL"),
    DRUG_CODE = c("DA", "DA", "DA", "DA"),
    DRUG = "Drug A",
    GENROICD = c("RT", "RT", "RT", "PR"),
    GSS = c(10, 20, 15, 9),
    stringsAsFactors = FALSE
  )
  drug_reference <- data.frame(
    DRUG_CODE = c("DA", "DB"),
    DRUG = c("Drug A", "Drug B"),
    GENROICD = c("RT", "RT"),
    stringsAsFactors = FALSE
  )
  category_rules <- data.frame(
    LOW = c(0, 1, 30),
    HIGH = c(0, 29.999, NA),
    CATEGORY = c("No penalty", "Some penalty", "High penalty"),
    CATEGORYN = c(0, 1, 2),
    stringsAsFactors = FALSE
  )

  result <- derive_hiv_stanford_gss(
    dat,
    stanford_rules,
    result_var = "GFSTRESC",
    key_vars = c("USUBJID", "VISITNUM"),
    region_var = "GFGENRI",
    region_map = region_map,
    drug_reference = drug_reference,
    category_rules = category_rules,
    regimen_drugs = c("DA", "DB"),
    total_drug_code = "REGTOT",
    total_drug_name = "Regimen Total",
    reference_version = "STANFORD-SYN"
  )

  da <- result[result$DRUG_CODE == "DA", ]
  db <- result[result$DRUG_CODE == "DB", ]
  total <- result[result$DRUG_CODE == "REGTOT", ]

  expect_equal(da$GSS_SINGLE, 20)
  expect_equal(da$GSS_COMBINATION, 15)
  expect_equal(da$GSS, 35)
  expect_equal(db$GSS, 0)
  expect_equal(total$GSS, 35)
  expect_equal(total$DRUG, "Regimen Total")
  expect_equal(da$AVALCAT1, "High penalty")
  expect_equal(db$AVALCAT1, "No penalty")
  expect_true(all(result$REFVER == "STANFORD-SYN"))
})

test_that("derive_hiv_stanford_gss expands compact multi-alternative rules", {
  dat <- data.frame(
    USUBJID = "SYN-002",
    VISITNUM = 1,
    GFSTRESC = "V11L",
    GFGENRI = "PR",
    stringsAsFactors = FALSE
  )
  region_map <- data.frame(
    REGION = "PR",
    GENROICD = "PR",
    GENROI = "PROTEASE",
    stringsAsFactors = FALSE
  )
  stanford_rules <- data.frame(
    GENOTYPE = "V11IL",
    DRUG_CODE = "DA",
    DRUG = "Drug A",
    GENROICD = "PR",
    GSS = 9,
    stringsAsFactors = FALSE
  )

  result <- derive_hiv_stanford_gss(
    dat,
    stanford_rules,
    result_var = "GFSTRESC",
    key_vars = c("USUBJID", "VISITNUM"),
    region_var = "GFGENRI",
    region_map = region_map
  )

  expect_equal(result$GSS, 9)
})

test_that("derive_hiv_stanford_gss prevents region-coded false matches", {
  dat <- data.frame(
    USUBJID = "SYN-003",
    VISITNUM = 1,
    GFSTRESC = "K103N",
    stringsAsFactors = FALSE
  )
  stanford_rules <- data.frame(
    GENOTYPE = c("K103N", "K103N"),
    DRUG_CODE = c("DA", "DB"),
    GENROICD = c("RT", "PR"),
    GSS = c(10, 50),
    stringsAsFactors = FALSE
  )

  expect_error(
    derive_hiv_stanford_gss(
      dat,
      stanford_rules,
      result_var = "GFSTRESC",
      key_vars = c("USUBJID", "VISITNUM")
    ),
    "both contain mapped region codes"
  )
})

test_that("derive_hiv_stanford_gss requires declared duplicate mutation handling", {
  dat <- data.frame(
    USUBJID = c("SYN-004", "SYN-004"),
    VISITNUM = c(1, 1),
    GFSTRESC = c("K103N", "K103N"),
    stringsAsFactors = FALSE
  )
  stanford_rules <- data.frame(
    GENOTYPE = "K103N",
    DRUG_CODE = "DA",
    GSS = 10,
    stringsAsFactors = FALSE
  )

  expect_error(
    derive_hiv_stanford_gss(
      dat,
      stanford_rules,
      result_var = "GFSTRESC",
      key_vars = c("USUBJID", "VISITNUM")
    ),
    "Duplicate parsed mutations"
  )

  result <- derive_hiv_stanford_gss(
    dat,
    stanford_rules,
    result_var = "GFSTRESC",
    key_vars = c("USUBJID", "VISITNUM"),
    duplicate_mutation_handling = "distinct"
  )
  expect_equal(result$GSS, 10)
})

test_that("derive_hiv_stanford_gss rejects incomplete and duplicate reference rules", {
  dat <- data.frame(
    USUBJID = c("SYN-005", "SYN-005"),
    VISITNUM = c(1, 1),
    GFSTRESC = c("K103N", "Y181C"),
    stringsAsFactors = FALSE
  )
  missing_score_rules <- data.frame(
    GENOTYPE = "K103N",
    DRUG_CODE = "DA",
    GSS = NA_real_,
    stringsAsFactors = FALSE
  )
  duplicate_combo_rules <- data.frame(
    GENOTYPE = c("K103N+Y181C", "K103N+Y181C"),
    DRUG_CODE = c("DA", "DA"),
    GSS = c(15, 15),
    stringsAsFactors = FALSE
  )
  repeated_component_rule <- data.frame(
    GENOTYPE = "K103N+K103N",
    DRUG_CODE = "DA",
    GSS = 15,
    stringsAsFactors = FALSE
  )
  trailing_delimiter_rule <- data.frame(
    GENOTYPE = "K103N+",
    DRUG_CODE = "DA",
    GSS = 15,
    stringsAsFactors = FALSE
  )
  overlapping_component_rule <- data.frame(
    GENOTYPE = "K103N/S+K103N",
    DRUG_CODE = "DA",
    GSS = 15,
    stringsAsFactors = FALSE
  )
  mixed_region_rules <- data.frame(
    GENOTYPE = c("K103N", "Y181C"),
    DRUG_CODE = c("DA", "DA"),
    GENROICD = c(NA, "RT"),
    GSS = c(10, 5),
    stringsAsFactors = FALSE
  )
  region_map <- data.frame(
    REGION = "RT",
    GENROICD = "RT",
    GENROI = "REVERSE TRANSCRIPTASE",
    stringsAsFactors = FALSE
  )

  expect_error(
    derive_hiv_stanford_gss(
      dat,
      missing_score_rules,
      result_var = "GFSTRESC",
      key_vars = c("USUBJID", "VISITNUM")
    ),
    "missing or non-finite penalty scores"
  )
  expect_error(
    derive_hiv_stanford_gss(
      dat,
      duplicate_combo_rules,
      result_var = "GFSTRESC",
      key_vars = c("USUBJID", "VISITNUM")
    ),
    "duplicate combination reference rules"
  )
  expect_error(
    derive_hiv_stanford_gss(
      dat[1, ],
      repeated_component_rule,
      result_var = "GFSTRESC",
      key_vars = c("USUBJID", "VISITNUM")
    ),
    "repeated components"
  )
  expect_error(
    derive_hiv_stanford_gss(
      dat[1, ],
      trailing_delimiter_rule,
      result_var = "GFSTRESC",
      key_vars = c("USUBJID", "VISITNUM")
    ),
    "empty combination component"
  )
  expect_error(
    derive_hiv_stanford_gss(
      dat[1, ],
      overlapping_component_rule,
      result_var = "GFSTRESC",
      key_vars = c("USUBJID", "VISITNUM")
    ),
    "overlapping components"
  )
  expect_error(
    derive_hiv_stanford_gss(
      cbind(dat[1, ], GFGENRI = "RT"),
      mixed_region_rules,
      result_var = "GFSTRESC",
      key_vars = c("USUBJID", "VISITNUM"),
      region_var = "GFGENRI",
      region_map = region_map
    ),
    "mixed missing and populated region codes"
  )
})
