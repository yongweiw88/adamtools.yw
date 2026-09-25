test_that("compare_adam_dataset detects no differences for identical data", {
  df <- data.frame(USUBJID = c("1", "2"), AVAL = c(1, 2))
  result <- compare_adam_dataset(df, df, keys = "USUBJID")
  expect_true(diffdf::diffdf_has_issues(result) == FALSE)
})

test_that("compare_adam_dataset detects value differences", {
  base <- data.frame(USUBJID = c("1", "2"), AVAL = c(1, 2))
  compare <- data.frame(USUBJID = c("1", "2"), AVAL = c(1, 99))
  result <- compare_adam_dataset(base, compare, keys = "USUBJID")
  expect_true(diffdf::diffdf_has_issues(result))
})

test_that("compare_adam_dataset strips attributes before comparing when requested", {
  base <- data.frame(USUBJID = c("1", "2"), AVAL = c(1, 2))
  compare <- base
  attr(compare$AVAL, "label") <- "Analysis Value"
  result <- compare_adam_dataset(base, compare, keys = "USUBJID", strip_attrs = TRUE)
  expect_true(diffdf::diffdf_has_issues(result) == FALSE)
})

test_that("compare_adam_dataset errors on non-data-frame input", {
  expect_error(compare_adam_dataset(1, data.frame(x = 1)), "must both be data frames")
})
