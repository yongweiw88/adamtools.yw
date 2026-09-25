test_that("derive_subject_baseline_values uses baseline-flagged record", {
  dat <- data.frame(
    USUBJID = c("1", "1", "2", "2"),
    TESTCD = c("WEIGHT", "WEIGHT", "WEIGHT", "WEIGHT"),
    AVAL = c(70, 72, 60, 62),
    ABLFL = c("Y", "", "Y", ""),
    VISIT = c("SCREENING", "WEEK 4", "SCREENING", "WEEK 4"),
    stringsAsFactors = FALSE
  )

  result <- derive_subject_baseline_values(dat, test_map = c(BWGTBL = "WEIGHT"))

  expect_equal(result$BWGTBL[result$USUBJID == "1"], 70)
  expect_equal(result$BWGTBL[result$USUBJID == "2"], 60)
})

test_that("derive_subject_baseline_values falls back to visit regex when no ABLFL", {
  dat <- data.frame(
    USUBJID = c("1", "1"),
    TESTCD = c("WEIGHT", "WEIGHT"),
    AVAL = c(70, 72),
    ABLFL = c("", ""),
    VISIT = c("SCREENING", "WEEK 4"),
    stringsAsFactors = FALSE
  )

  result <- derive_subject_baseline_values(
    dat,
    test_map = c(BWGTBL = "WEIGHT"),
    fallback_visit_regex = "SCREEN"
  )

  expect_equal(result$BWGTBL, 70)
})

test_that("derive_subject_baseline_values errors on non-named test_map", {
  dat <- data.frame(USUBJID = "1", TESTCD = "WEIGHT", AVAL = 1, ABLFL = "Y", stringsAsFactors = FALSE)
  expect_error(derive_subject_baseline_values(dat, test_map = c("WEIGHT")), "named")
})
