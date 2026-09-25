test_that("derive_hiv_resistance_category applies numeric fold-change rules", {
  dat <- data.frame(
    DRUG = c("ABC", "ABC", "ABC"),
    FOLDCHG = c(4.5, 4.6, 7)
  )
  rules <- data.frame(
    DRUG = c("ABC", "ABC", "ABC"),
    LOW = c(0, 4.5, 6.5),
    HIGH = c(4.5, 6.5, NA),
    LOW_INCL = c(TRUE, FALSE, FALSE),
    HIGH_INCL = c(TRUE, TRUE, TRUE),
    CATEGORY = c("Sensitive", "Partially Sensitive", "Resistant"),
    CATEGORYN = c(3, 2, 1)
  )

  result <- derive_hiv_resistance_category(
    dat,
    rules,
    value_var = "FOLDCHG",
    match_vars = "DRUG",
    lower_inclusive_var = "LOW_INCL",
    upper_inclusive_var = "HIGH_INCL",
    category_var = "AVALCAT4",
    ordinal_var = "AVALCT4N"
  )

  expect_equal(result$AVALCAT4, c("Sensitive", "Partially Sensitive", "Resistant"))
  expect_equal(result$AVALCT4N, c(3, 2, 1))
})

test_that("derive_hiv_resistance_category applies categorical source-result rules", {
  dat <- data.frame(
    TEST = c("GENO", "GENO", "PHENO"),
    SRCRES = c("S", "R", "I")
  )
  rules <- data.frame(
    TEST = c("GENO", "GENO", "PHENO"),
    SOURCE_VALUE = c("S", "R", "I"),
    CATEGORY = c("Sensitive", "Resistant", "Intermediate"),
    CATEGORYN = c(3, 1, 2)
  )

  result <- derive_hiv_resistance_category(
    dat,
    rules,
    source_result_var = "SRCRES",
    match_vars = "TEST"
  )

  expect_equal(result$RESCAT, c("Sensitive", "Resistant", "Intermediate"))
  expect_equal(result$RESCATN, c(3, 1, 2))
})

test_that("derive_hiv_resistance_category errors on unmatched and overlapping rules", {
  dat <- data.frame(DRUG = "ABC", FOLDCHG = 5)
  unmatched_rules <- data.frame(
    DRUG = "ABC",
    LOW = 0,
    HIGH = 4,
    CATEGORY = "Sensitive",
    CATEGORYN = 3
  )
  overlapping_rules <- data.frame(
    DRUG = c("ABC", "ABC"),
    LOW = c(0, 4),
    HIGH = c(6, 8),
    CATEGORY = c("A", "B"),
    CATEGORYN = c(1, 2)
  )

  expect_error(
    derive_hiv_resistance_category(dat, unmatched_rules, value_var = "FOLDCHG", match_vars = "DRUG"),
    "did not match"
  )
  expect_error(
    derive_hiv_resistance_category(dat, overlapping_rules, value_var = "FOLDCHG", match_vars = "DRUG"),
    "multiple"
  )
})
