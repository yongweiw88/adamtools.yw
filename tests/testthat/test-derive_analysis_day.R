test_that("derive_analysis_day computes analysis day using the ADaM day convention (no Day 0)", {
  df <- data.frame(
    TRTSDT = as.Date(c("2024-01-01", "2024-01-01", "2024-01-01", "2024-01-01")),
    ADT = as.Date(c("2024-01-01", "2024-01-05", "2023-12-30", "2023-12-27")),
    stringsAsFactors = FALSE
  )

  result <- derive_analysis_day(df, ADT, TRTSDT)

  # Reference date itself is day 1 (no Day 0); on/after -> diff + 1; before -> raw negative diff
  expect_equal(result$ADY, c(1, 5, -2, -5))
})

test_that("derive_analysis_day supports custom output variable names", {
  df <- data.frame(
    baseline_dt = as.Date(c("2024-02-01", "2024-02-01")),
    visit_dt = as.Date(c("2024-02-03", "2024-02-15")),
    stringsAsFactors = FALSE
  )

  result <- derive_analysis_day(df, visit_dt, baseline_dt, new_var = "AVISITDY")

  expect_equal(result$AVISITDY, c(3, 15))
})

test_that("derive_analysis_day errors when a required column is missing", {
  df <- data.frame(
    ADT = as.Date(c("2024-01-02")),
    stringsAsFactors = FALSE
  )

  expect_error(derive_analysis_day(df, ADT, TRTSDT), "`anchor_var` is not a column")
})
