test_that("derive_hiv_ias_resistance matches observed mutations to the dictionary", {
  dictionary <- data.frame(
    GENOTYPE = "K103N",
    CLASS = "NNRTI",
    SUBCLASS = "MAJOR",
    DRUG_CODE = "EFV",
    DRUG_NAME = "Efavirenz",
    stringsAsFactors = FALSE
  )
  dat <- data.frame(
    USUBJID = c("1", "2"),
    GFSTRESC = c("K103N", "M184V"),
    stringsAsFactors = FALSE
  )

  result <- derive_hiv_ias_resistance(
    dat,
    ias_dictionary = dictionary,
    result_var = GFSTRESC,
    key_vars = "USUBJID"
  )

  matched <- result[result$USUBJID == "1", ]
  unmatched <- result[result$USUBJID == "2", ]

  expect_equal(matched$MATCHFL, "Y")
  expect_equal(matched$IAS_CLASS, "NNRTI")
  expect_equal(matched$IAS_CLASSIFICATION, "MAJOR")
  expect_true(is.na(unmatched$MATCHFL))
})

test_that("derive_hiv_ias_resistance derives generic MCRIT variables for major mutations", {
  dictionary <- data.frame(
    GENOTYPE = "K103N",
    CLASS = "NNRTI",
    SUBCLASS = "MAJOR",
    stringsAsFactors = FALSE
  )
  dat <- data.frame(GFSTRESC = "K103N", stringsAsFactors = FALSE)

  result <- derive_hiv_ias_resistance(dat, ias_dictionary = dictionary, result_var = GFSTRESC)

  expect_equal(result$MCRIT1, "Any Mutation")
  expect_equal(result$MCRIT3, "Major Mutation Class")
  expect_true(is.na(result$MCRIT2))
  expect_equal(result$MCRIT3ML, "NNRTI")
})

test_that("derive_hiv_ias_resistance can skip MCRIT derivation", {
  dictionary <- data.frame(
    GENOTYPE = "K103N",
    CLASS = "NNRTI",
    SUBCLASS = "MAJOR",
    stringsAsFactors = FALSE
  )
  dat <- data.frame(GFSTRESC = "K103N", stringsAsFactors = FALSE)

  result <- derive_hiv_ias_resistance(
    dat,
    ias_dictionary = dictionary,
    result_var = GFSTRESC,
    derive_mcrit = FALSE
  )

  expect_false("MCRIT1" %in% names(result))
})

test_that("derive_hiv_ias_resistance treats caller-supplied prespecified rules as specified mutations", {
  dictionary <- data.frame(
    GENOTYPE = "K103N",
    CLASS = "NNRTI",
    SUBCLASS = "MAJOR",
    stringsAsFactors = FALSE
  )
  prespecified <- data.frame(GENOTYPE = "E138K", stringsAsFactors = FALSE)
  dat <- data.frame(GFSTRESC = "E138K", stringsAsFactors = FALSE)

  result <- derive_hiv_ias_resistance(
    dat,
    ias_dictionary = dictionary,
    result_var = GFSTRESC,
    prespecified_rules = prespecified
  )

  expect_equal(result$IAS_CLASSIFICATION, "PRESP")
  expect_equal(result$MCRIT2, "Specified Mutation")
})

test_that("derive_hiv_ias_resistance errors on unparseable dictionary rules by default", {
  dictionary <- data.frame(
    GENOTYPE = c("K103N", ""),
    CLASS = "NNRTI",
    SUBCLASS = "MAJOR",
    stringsAsFactors = FALSE
  )
  dat <- data.frame(GFSTRESC = "K103N", stringsAsFactors = FALSE)

  expect_error(
    derive_hiv_ias_resistance(dat, ias_dictionary = dictionary, result_var = GFSTRESC),
    "unparseable"
  )
})

test_that("derive_hiv_ias_resistance validates required inputs are data frames", {
  dat <- data.frame(GFSTRESC = "K103N", stringsAsFactors = FALSE)

  expect_error(
    derive_hiv_ias_resistance(dat, ias_dictionary = list(), result_var = GFSTRESC),
    "data frame"
  )
})
