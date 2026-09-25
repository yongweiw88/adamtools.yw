test_that("derive_isr_events keeps source rows and appends configured aggregates", {
  ae <- data.frame(
    USUBJID = c("01", "01", "02", "03"),
    ISRFLAG = c("Y", "Y", "N", "Y"),
    AECAT = c("LOCAL", "LOCAL", "LOCAL", "SYSTEMIC"),
    AEDECOD = c("Nimbus", "Quartz", "Nimbus", "Orchid"),
    AEDECODN = c(10, 20, 10, 30),
    stringsAsFactors = FALSE
  )
  rules <- data.frame(
    source_term = c(NA, "Nimbus", "Quartz"),
    source_category = c("LOCAL", "LOCAL", "LOCAL"),
    derived_term = c("Any configured local event", "Configured grouped event", "Configured grouped event"),
    derived_category = c("LOCAL", "LOCAL", "LOCAL"),
    derived_order = c(1, 99, 99),
    dtype = c("DERIVED", "DERIVED", "DERIVED"),
    stringsAsFactors = FALSE
  )

  result <- derive_isr_events(
    ae,
    term_var = "AEDECOD",
    category_var = "AECAT",
    qualifier_var = "ISRFLAG",
    qualifier_values = "Y",
    category_values = "LOCAL",
    aggregate_rules = rules,
    order_var = "AEDECODN"
  )

  expect_equal(nrow(result), 6L)
  expect_equal(result$USUBJID[1:2], c("01", "01"))
  expect_equal(result$AEDECOD[1:2], c("Nimbus", "Quartz"))
  expect_true(all(result$DTYPE[3:6] == "DERIVED"))
  expect_equal(result$AEDECODN[result$AEDECOD == "Any configured local event"], c(1, 1))
  expect_equal(sum(result$AEDECOD == "Configured grouped event"), 2L)
  expect_false(any(grepl("INJECTION|INFUSION|INDURATION|SWELLING", result$AEDECOD, ignore.case = TRUE)))
})

test_that("derive_isr_events validates duplicate rules and unmatched policies", {
  ae <- data.frame(
    ISRFLAG = c("Y", "Y"),
    AECAT = c("A", "A"),
    AEDECOD = c("Term A", "Term B"),
    stringsAsFactors = FALSE
  )

  duplicate_rules <- data.frame(
    source_term = c("Term A", "Term A"),
    source_category = c("A", "A"),
    derived_term = c("Aggregate", "Aggregate"),
    stringsAsFactors = FALSE
  )
  expect_error(
    derive_isr_events(
      ae,
      term_var = "AEDECOD",
      category_var = "AECAT",
      qualifier_var = "ISRFLAG",
      aggregate_rules = duplicate_rules
    ),
    "duplicate aggregate rule"
  )

  missing_rule <- data.frame(
    source_term = "Not observed",
    source_category = "A",
    derived_term = "Aggregate",
    stringsAsFactors = FALSE
  )
  expect_error(
    derive_isr_events(
      ae,
      term_var = "AEDECOD",
      category_var = "AECAT",
      qualifier_var = "ISRFLAG",
      aggregate_rules = missing_rule
    ),
    "did not match"
  )

  partial_rules <- data.frame(
    source_term = "Term A",
    source_category = "A",
    derived_term = "Aggregate",
    stringsAsFactors = FALSE
  )
  expect_error(
    derive_isr_events(
      ae,
      term_var = "AEDECOD",
      category_var = "AECAT",
      qualifier_var = "ISRFLAG",
      aggregate_rules = partial_rules,
      unmatched_rules = "ignore",
      unmatched_terms = "error"
    ),
    "did not match any aggregate rule"
  )
})
