test_that("carry_subject_vars carries consistent values and NAs conflicts", {
  dat <- data.frame(
    USUBJID = c("1", "1", "2", "2"),
    SEX = c("F", "F", "M", "F"),
    AGE = c(30, 30, 40, 40),
    stringsAsFactors = FALSE
  )

  result <- carry_subject_vars(dat, vars = c("SEX", "AGE"))

  expect_equal(result$SEX[result$USUBJID == "1"], "F")
  expect_true(is.na(result$SEX[result$USUBJID == "2"]))
  expect_equal(result$AGE[result$USUBJID == "2"], 40)
})

test_that("carry_subject_vars ignores missing values when checking consistency", {
  dat <- data.frame(
    USUBJID = c("1", "1"),
    FLAG = c("Y", NA),
    stringsAsFactors = FALSE
  )
  result <- carry_subject_vars(dat, vars = "FLAG")
  expect_equal(result$FLAG, "Y")
})

test_that("carry_subject_vars errors on missing vars", {
  dat <- data.frame(USUBJID = "1", stringsAsFactors = FALSE)
  expect_error(carry_subject_vars(dat, vars = "NOPE"), "not found")
})
