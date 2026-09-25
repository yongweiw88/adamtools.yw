test_that("derive_duration computes inclusive day count", {
  dat <- data.frame(
    ASTDT = as.Date(c("2023-01-01", "2023-01-01")),
    AENDT = as.Date(c("2023-01-10", NA)),
    stringsAsFactors = FALSE
  )

  result <- derive_duration(dat, ASTDT, AENDT)

  expect_equal(result$ADURN[1], 10)
  expect_equal(result$ADURU[1], "DAYS")
  expect_true(is.na(result$ADURN[2]))
})

test_that("derive_duration supports cutoff_date and other units", {
  dat <- data.frame(
    ASTDT = as.Date("2023-01-01"),
    AENDT = as.Date(NA),
    CUTOFF = as.Date("2023-01-15"),
    stringsAsFactors = FALSE
  )

  result <- derive_duration(dat, ASTDT, AENDT, unit = "WEEKS", cutoff_date = CUTOFF, round_down = TRUE)

  expect_equal(result$ADURN, floor(15 / 7))
  expect_equal(result$ADURU, "WEEKS")
})

test_that("derive_duration errors on missing columns", {
  dat <- data.frame(x = 1)
  expect_error(derive_duration(dat, ASTDT, AENDT), "start_var")
})
