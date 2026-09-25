test_that("derive_hiv_stanford_gss sums single-mutation and combination penalty scores", {
  rules <- data.frame(
    GENOTYPE = c("K103N", "M184V", "K103N+M184V"),
    DRUG_CODE = "EFV",
    GSS = c(1, 0.5, 2),
    stringsAsFactors = FALSE
  )
  dat <- data.frame(
    USUBJID = c("1", "1"),
    GFSTRESC = c("K103N", "M184V"),
    stringsAsFactors = FALSE
  )

  result <- derive_hiv_stanford_gss(
    dat,
    stanford_rules = rules,
    result_var = GFSTRESC,
    key_vars = "USUBJID"
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$DRUG_CODE, "EFV")
  expect_equal(result$GSS_SINGLE, 1.5)
  expect_equal(result$GSS_COMBINATION, 2)
  expect_equal(result$GSS, 3.5)
})

test_that("derive_hiv_stanford_gss only applies a combination rule when every component is observed", {
  rules <- data.frame(
    GENOTYPE = c("K103N", "M184V", "K103N+M184V"),
    DRUG_CODE = "EFV",
    GSS = c(1, 0.5, 2),
    stringsAsFactors = FALSE
  )
  dat <- data.frame(USUBJID = "1", GFSTRESC = "K103N", stringsAsFactors = FALSE)

  result <- derive_hiv_stanford_gss(
    dat,
    stanford_rules = rules,
    result_var = GFSTRESC,
    key_vars = "USUBJID"
  )

  expect_equal(result$GSS_SINGLE, 1)
  expect_equal(result$GSS_COMBINATION, 0)
  expect_equal(result$GSS, 1)
})

test_that("derive_hiv_stanford_gss adds a regimen total row when regimen_drugs is supplied", {
  rules <- data.frame(
    GENOTYPE = c("K103N", "M184V"),
    DRUG_CODE = c("EFV", "3TC"),
    GSS = c(1, 0.5),
    stringsAsFactors = FALSE
  )
  dat <- data.frame(
    USUBJID = c("1", "1"),
    GFSTRESC = c("K103N", "M184V"),
    stringsAsFactors = FALSE
  )

  result <- derive_hiv_stanford_gss(
    dat,
    stanford_rules = rules,
    result_var = GFSTRESC,
    key_vars = "USUBJID",
    regimen_drugs = c("EFV", "3TC")
  )

  total <- result[result$DRUG_CODE == "TOTAL", ]
  expect_equal(nrow(total), 1L)
  expect_equal(total$GSS, 1.5)
})

test_that("derive_hiv_stanford_gss can categorize the derived score via derive_hiv_resistance_category", {
  rules <- data.frame(
    GENOTYPE = c("K103N", "M184V", "K103N+M184V"),
    DRUG_CODE = "EFV",
    GSS = c(1, 0.5, 2),
    stringsAsFactors = FALSE
  )
  dat <- data.frame(
    USUBJID = c("1", "1"),
    GFSTRESC = c("K103N", "M184V"),
    stringsAsFactors = FALSE
  )
  category_rules <- data.frame(
    LOW = c(0, 2),
    HIGH = c(2, NA),
    CATEGORY = c("Susceptible", "Resistant"),
    CATEGORYN = c(2, 1)
  )

  result <- derive_hiv_stanford_gss(
    dat,
    stanford_rules = rules,
    result_var = GFSTRESC,
    key_vars = "USUBJID",
    category_rules = category_rules
  )

  expect_equal(result$AVALCAT1, "Resistant")
  expect_equal(result$AVALCAT1N, 1)
})

test_that("derive_hiv_stanford_gss errors on duplicate mutations unless told to collapse them", {
  rules <- data.frame(
    GENOTYPE = "K103N",
    DRUG_CODE = "EFV",
    GSS = 1,
    stringsAsFactors = FALSE
  )
  dat <- data.frame(
    USUBJID = c("1", "1"),
    GFSTRESC = c("K103N", "K103N"),
    stringsAsFactors = FALSE
  )

  expect_error(
    derive_hiv_stanford_gss(dat, stanford_rules = rules, result_var = GFSTRESC, key_vars = "USUBJID"),
    "Duplicate parsed mutations"
  )

  result <- derive_hiv_stanford_gss(
    dat,
    stanford_rules = rules,
    result_var = GFSTRESC,
    key_vars = "USUBJID",
    duplicate_mutation_handling = "distinct"
  )
  expect_equal(result$GSS, 1)
})

test_that("derive_hiv_stanford_gss validates that stanford_rules scores are numeric and complete", {
  dat <- data.frame(USUBJID = "1", GFSTRESC = "K103N", stringsAsFactors = FALSE)
  bad_rules <- data.frame(
    GENOTYPE = "K103N",
    DRUG_CODE = "EFV",
    GSS = NA_real_,
    stringsAsFactors = FALSE
  )

  expect_error(
    derive_hiv_stanford_gss(dat, stanford_rules = bad_rules, result_var = GFSTRESC, key_vars = "USUBJID"),
    "missing or non-finite"
  )
})
