test_that("pmax_date_na returns rowwise max and preserves Date class", {
  d1 <- as.Date(c("2021-01-01", NA, "2021-03-01"))
  d2 <- as.Date(c("2021-02-01", NA, NA))
  result <- pmax_date_na(d1, d2)
  expect_s3_class(result, "Date")
  expect_equal(result, as.Date(c("2021-02-01", NA, "2021-03-01")))
})

test_that("pmin_date_na returns rowwise min", {
  d1 <- as.Date(c("2021-01-01", "2021-05-01"))
  d2 <- as.Date(c("2021-02-01", "2021-01-01"))
  result <- pmin_date_na(d1, d2)
  expect_equal(result, as.Date(c("2021-01-01", "2021-01-01")))
})

test_that("pmin_date_na uses all_na for fully missing rows", {
  d1 <- as.Date(c(NA, "2021-01-01"))
  d2 <- as.Date(c(NA, NA))
  result <- pmin_date_na(d1, d2, all_na = as.Date("1900-01-01"))
  expect_equal(result, as.Date(c("1900-01-01", "2021-01-01")))
})

test_that("p_date_extreme errors when no vectors supplied", {
  expect_error(p_date_extreme(), "At least one date vector")
})

test_that("p_date_extreme errors on mismatched lengths", {
  expect_error(
    p_date_extreme(as.Date("2021-01-01"), as.Date(c("2021-01-01", "2021-01-02"))),
    "same length"
  )
})
