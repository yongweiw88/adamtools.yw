test_that("join_adsl_vars joins selected ADSL variables by subject", {
  adsl <- data.frame(
    STUDYID = c("STUDY01", "STUDY01"),
    USUBJID = c("STUDY01-001", "STUDY01-002"),
    SUBJID = c("001", "002"),
    TRT01A = c("Drug A", "Drug B"),
    AGE = c(45, 50),
    stringsAsFactors = FALSE
  )

  dataset <- data.frame(
    STUDYID = c("STUDY01", "STUDY01"),
    USUBJID = c("STUDY01-001", "STUDY01-002"),
    AVAL = c(10, 20),
    stringsAsFactors = FALSE
  )

  result <- join_adsl_vars(dataset, adsl, vars = c("TRT01A", "AGE"))

  expect_equal(result$TRT01A, c("Drug A", "Drug B"))
  expect_equal(result$AGE, c(45, 50))
  expect_equal(names(result), c("STUDYID", "USUBJID", "AVAL", "TRT01A", "AGE"))
})

test_that("join_adsl_vars defaults to all ADSL variables except join keys and SUBJID", {
  adsl <- data.frame(
    STUDYID = c("STUDY01", "STUDY01"),
    USUBJID = c("STUDY01-001", "STUDY01-002"),
    SUBJID = c("001", "002"),
    TRT01A = c("Drug A", "Drug B"),
    AGE = c(45, 50),
    stringsAsFactors = FALSE
  )

  dataset <- data.frame(
    STUDYID = c("STUDY01", "STUDY01"),
    USUBJID = c("STUDY01-001", "STUDY01-002"),
    AVAL = c(10, 20),
    stringsAsFactors = FALSE
  )

  result <- join_adsl_vars(dataset, adsl)

  expect_true(all(c("TRT01A", "AGE") %in% names(result)))
  expect_false("SUBJID" %in% names(result))
})

test_that("join_adsl_vars warns and skips duplicate variable names", {
  adsl <- data.frame(
    STUDYID = c("STUDY01", "STUDY01"),
    USUBJID = c("STUDY01-001", "STUDY01-002"),
    AGE = c(45, 50),
    stringsAsFactors = FALSE
  )

  dataset <- data.frame(
    STUDYID = c("STUDY01", "STUDY01"),
    USUBJID = c("STUDY01-001", "STUDY01-002"),
    AGE = c(99, 88),
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- join_adsl_vars(dataset, adsl, vars = c("AGE")),
    "already existed in `dataset`"
  )

  expect_equal(result$AGE, c(99, 88))
})
