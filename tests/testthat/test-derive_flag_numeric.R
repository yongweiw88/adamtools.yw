test_that("derive_flag_numeric maps Y/N flags to 1/0", {
  df <- data.frame(SAFFL = c("Y", "N", NA), FASFL = c("N", "Y", "Y"))
  result <- derive_flag_numeric(df, mappings = c(SAFFL = "SAFFN", FASFL = "FASFN"))

  expect_equal(result$SAFFN, c(1, 0, NA_real_))
  expect_equal(result$FASFN, c(0, 1, 1))
})

test_that("derive_flag_numeric supports custom no/missing values", {
  df <- data.frame(FL = c("Y", "N", NA))
  result <- derive_flag_numeric(df, mappings = c(FL = "FN"), no = 2, missing = -1)
  expect_equal(result$FN, c(1, 2, -1))
})

test_that("derive_flag_numeric errors on missing source variable", {
  df <- data.frame(x = 1)
  expect_error(
    derive_flag_numeric(df, mappings = c(SAFFL = "SAFFN")),
    "not in `dataset`"
  )
})

test_that("derive_flag_numeric errors when mappings is not named", {
  df <- data.frame(SAFFL = "Y")
  expect_error(derive_flag_numeric(df, mappings = "SAFFL"), "named character vector")
})
