test_that("derive_log_change_vars derives log value only when base is NULL", {
  df <- data.frame(AVAL = c(1, exp(1), 10))
  result <- derive_log_change_vars(df, base = NULL)
  expect_equal(result$LOGVAL, c(0, 1, log(10)))
  expect_false("LOGBASE" %in% names(result))
})

test_that("derive_log_change_vars derives log baseline and change", {
  df <- data.frame(AVAL = c(10, 100), BASE = c(1, 10))
  result <- derive_log_change_vars(df, base_fn = log10)
  expect_equal(result$LOGVAL, c(1, 2))
  expect_equal(result$LOGBASE, c(0, 1))
  expect_equal(result$LOGCHG, c(1, 1))
})

test_that("derive_log_change_vars sets non-positive values to NA when require_positive", {
  df <- data.frame(AVAL = c(0, -1, 5))
  result <- derive_log_change_vars(df, base = NULL)
  expect_equal(result$LOGVAL, c(NA_real_, NA_real_, log(5)))
})

test_that("derive_log_change_vars errors on missing aval column", {
  df <- data.frame(x = 1)
  expect_error(derive_log_change_vars(df, aval = AVAL, base = NULL), "not a column")
})
