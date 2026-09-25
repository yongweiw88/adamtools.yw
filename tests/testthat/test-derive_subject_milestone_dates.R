test_that("derive_subject_milestone_dates finds earliest matching date per subject", {
  ds <- data.frame(
    USUBJID = c("1", "1", "2"),
    DSDECOD = c("RANDOMIZED", "SCREEN FAILURE", "RANDOMIZED"),
    DSSTDTC = c("2023-01-10", "2023-01-05", "2023-02-01"),
    stringsAsFactors = FALSE
  )

  result <- derive_subject_milestone_dates(
    ds,
    milestones = list(RANDDT = "RANDOMIZED", SCRFDT = c("SCREEN FAILURE", "SCREEN FAILED"))
  )

  expect_equal(result$RANDDT[result$USUBJID == "1"], as.Date("2023-01-10"))
  expect_equal(result$RANDDT[result$USUBJID == "2"], as.Date("2023-02-01"))
  expect_equal(result$SCRFDT[result$USUBJID == "1"], as.Date("2023-01-05"))
  expect_true(is.na(result$SCRFDT[result$USUBJID == "2"]))
})

test_that("derive_subject_milestone_dates errors on unnamed milestones", {
  ds <- data.frame(USUBJID = "1", DSDECOD = "RANDOMIZED", DSSTDTC = "2023-01-01", stringsAsFactors = FALSE)
  expect_error(derive_subject_milestone_dates(ds, milestones = list("RANDOMIZED")), "named list")
})
