test_that("derive_treatment_dates derives overall TRTSDT/TRTEDT excluding zero dose", {
  ex <- data.frame(
    USUBJID = c("1", "1", "1", "2"),
    EXDOSE = c(0, 10, 10, 10),
    EXSTDTC = c("2023-01-01", "2023-01-02", "2023-01-15", "2023-02-01"),
    EXENDTC = c("2023-01-01", "2023-01-02", "2023-01-20", "2023-02-05"),
    stringsAsFactors = FALSE
  )

  result <- derive_treatment_dates(ex)

  expect_equal(result$TRTSDT[result$USUBJID == "1"], as.Date("2023-01-02"))
  expect_equal(result$TRTEDT[result$USUBJID == "1"], as.Date("2023-01-20"))
})

test_that("derive_treatment_dates uses EXSTDTC for single-day records without EXENDTC", {
  ex <- data.frame(
    USUBJID = c("1", "1", "2"),
    EXDOSE = c(10, 10, 10),
    EXSTDTC = c("2023-01-01", "2023-01-15", "2023-02-01"),
    EXENDTC = c("", NA, "2023-02-05"),
    stringsAsFactors = FALSE
  )

  result <- derive_treatment_dates(ex)

  expect_equal(result$TRTSDT[result$USUBJID == "1"], as.Date("2023-01-01"))
  expect_equal(result$TRTEDT[result$USUBJID == "1"], as.Date("2023-01-15"))
  expect_equal(result$TRTEDT[result$USUBJID == "2"], as.Date("2023-02-05"))
})

test_that("derive_treatment_dates derives per-period dates", {
  ex <- data.frame(
    USUBJID = c("1", "1"),
    EXDOSE = c(10, 10),
    EXSTDTC = c("2023-01-01", "2023-02-01"),
    EXENDTC = c("2023-01-10", "2023-02-10"),
    APERIOD = c(1, 2),
    stringsAsFactors = FALSE
  )

  result <- derive_treatment_dates(ex, period_var = "APERIOD")

  expect_equal(result$TRT01SDT, as.Date("2023-01-01"))
  expect_equal(result$TRT02SDT, as.Date("2023-02-01"))
})

test_that("derive_treatment_dates requires epoch_map with epoch_var", {
  ex <- data.frame(
    USUBJID = "1", EXDOSE = 10, EXSTDTC = "2023-01-01", EXENDTC = "2023-01-05",
    EPOCH = "TREATMENT 1", stringsAsFactors = FALSE
  )
  expect_error(derive_treatment_dates(ex, epoch_var = "EPOCH"), "epoch_map")
})
