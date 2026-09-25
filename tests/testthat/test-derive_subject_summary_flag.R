test_that("derive_subject_summary_flag applies positive over negative precedence", {
  dat <- data.frame(
    USUBJID = c("1", "1", "2", "3", "4"),
    RESULT = c("NEG", "POS", "NEG", "IND", NA),
    stringsAsFactors = FALSE
  )

  result <- derive_subject_summary_flag(
    dat,
    flag_var = "ANYFL",
    positive = RESULT == "POS",
    negative = RESULT == "NEG"
  )

  expect_equal(result$ANYFL[match(c("1", "2", "3", "4"), result$USUBJID)], c("Y", "N", "", ""))
})

test_that("derive_subject_summary_flag supports custom values and grouping", {
  dat <- data.frame(
    USUBJID = c("1", "1", "1"),
    TESTCD = c("A", "A", "B"),
    STATUS = c("NON-REACTIVE", "REACTIVE", "NON-REACTIVE"),
    stringsAsFactors = FALSE
  )

  result <- derive_subject_summary_flag(
    dat,
    by = c("USUBJID", "TESTCD"),
    flag_var = "STATUS_SUM",
    positive = STATUS == "REACTIVE",
    negative = STATUS == "NON-REACTIVE",
    positive_value = "Positive",
    negative_value = "Negative",
    missing_value = ""
  )

  expect_equal(result$STATUS_SUM[match(c("A", "B"), result$TESTCD)], c("Positive", "Negative"))
})

test_that("derive_subject_summary_flag validates logical conditions", {
  dat <- data.frame(USUBJID = "1", RESULT = "POS", stringsAsFactors = FALSE)

  expect_error(
    derive_subject_summary_flag(dat, "FL", positive = RESULT),
    "logical vector"
  )
})
