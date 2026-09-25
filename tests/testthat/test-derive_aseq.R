test_that("derive_aseq sequences over whole dataset", {
  df <- data.frame(USUBJID = c("1", "1", "2"))
  result <- derive_aseq(df)
  expect_equal(result$ASEQ, c(1, 2, 3))
  expect_type(result$ASEQ, "double")
})

test_that("derive_aseq sequences within groups", {
  df <- data.frame(USUBJID = c("1", "1", "2", "2", "2"))
  result <- derive_aseq(df, by = "USUBJID")
  expect_equal(result$ASEQ, c(1, 2, 1, 2, 3))
})

test_that("derive_aseq supports integer output and custom var name", {
  df <- data.frame(x = 1:3)
  result <- derive_aseq(df, var = SEQ, as_numeric = FALSE)
  expect_equal(result$SEQ, 1:3)
  expect_type(result$SEQ, "integer")
})

test_that("derive_aseq errors on missing by variable", {
  df <- data.frame(x = 1:3)
  expect_error(derive_aseq(df, by = "USUBJID"), "not in `dataset`")
})
