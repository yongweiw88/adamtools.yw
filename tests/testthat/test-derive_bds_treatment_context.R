test_that("derive_bds_treatment_context assigns phase and period", {
  dat <- data.frame(
    USUBJID = c("1", "1", "1", "2"),
    ADT = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-02-15")),
    TRTSDT = as.Date(c("2023-01-15", "2023-01-15", "2023-01-15", "2023-02-01")),
    P1START = as.Date(c("2023-01-15", "2023-01-15", "2023-01-15", "2023-02-01")),
    P2START = as.Date(c("2023-02-20", "2023-02-20", "2023-02-20", NA)),
    stringsAsFactors = FALSE
  )
  period_map <- data.frame(
    period = c(1, 2),
    start_var = c("P1START", "P2START"),
    label = c("Period 1", "Period 2"),
    stringsAsFactors = FALSE
  )

  result <- derive_bds_treatment_context(dat, period_map = period_map)

  expect_equal(result$APHASE[1], "Pre-Treatment")
  expect_equal(result$APHASE[2], "On-Treatment")
  expect_equal(result$APERIOD[2], 1)
  expect_equal(result$APERIOD[3], 2)
  expect_equal(result$APERIODC[3], "Period 2")
})

test_that("derive_bds_treatment_context derives TRTP/TRTA per period", {
  dat <- data.frame(
    USUBJID = c("1", "1"),
    ADT = as.Date(c("2023-02-01", "2023-03-01")),
    TRTSDT = as.Date(c("2023-01-15", "2023-01-15")),
    P1START = as.Date(c("2023-01-15", "2023-01-15")),
    P2START = as.Date(c("2023-02-20", "2023-02-20")),
    TRT01P = c("Drug A", "Drug A"),
    TRT02P = c("Drug B", "Drug B"),
    stringsAsFactors = FALSE
  )
  period_map <- data.frame(period = c(1, 2), start_var = c("P1START", "P2START"), stringsAsFactors = FALSE)

  result <- derive_bds_treatment_context(
    dat,
    period_map = period_map,
    trtp_map = c("1" = "TRT01P", "2" = "TRT02P")
  )

  expect_equal(result$TRTP[1], "Drug A")
  expect_equal(result$TRTP[2], "Drug B")
})

test_that("derive_bds_treatment_context errors on invalid input", {
  expect_error(derive_bds_treatment_context(list(a = 1)), "data frame")
  expect_error(derive_bds_treatment_context(data.frame(ADT = 1), trt_start_var = "TRTSDT"), "trt_start_var")
})
