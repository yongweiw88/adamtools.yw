test_that("derive_first_occurrence_flag flags first eligible record per group", {
  df <- data.frame(
    USUBJID = c("1", "1", "1", "2"),
    ASTDT = as.Date(c("2021-01-05", "2021-01-01", "2021-01-10", "2021-01-01")),
    TRTEMFL = c("Y", "Y", "Y", "N")
  )

  result <- derive_first_occurrence_flag(
    df,
    flag = "AOCCFL",
    by = "USUBJID",
    order = "ASTDT",
    condition = df$TRTEMFL == "Y"
  )

  expect_equal(result$AOCCFL, c(NA, "Y", NA, NA))
})

test_that("derive_first_occurrence_flag preserves row order", {
  df <- data.frame(USUBJID = c("2", "1", "1"), x = 1:3)
  result <- derive_first_occurrence_flag(df, by = "USUBJID")
  expect_equal(result$x, 1:3)
  expect_equal(result$AOCCFL, c("Y", "Y", NA))
})

test_that("derive_first_occurrence_flag errors on missing by variable", {
  df <- data.frame(x = 1)
  expect_error(derive_first_occurrence_flag(df, by = "USUBJID"), "not in `dataset`")
})
