test_that("apply_lookup maps values using a named vector", {
  df <- data.frame(
    COUNTRY = c("MEX", "ARG", "GBR", "XXX"),
    stringsAsFactors = FALSE
  )

  cntry_to_acntry <- c(
    "MEX" = "Mexico",
    "ARG" = "Argentina",
    "GBR" = "United Kingdom"
  )

  result <- apply_lookup(df, COUNTRY, ACOUNTRY, cntry_to_acntry)

  expect_equal(result$ACOUNTRY, c("Mexico", "Argentina", "United Kingdom", NA_character_))
})

test_that("apply_lookup maps values using a two-column data frame", {
  df <- data.frame(
    SEX = c("M", "F", "M"),
    stringsAsFactors = FALSE
  )

  lookup_df <- data.frame(
    key = c("M", "F"),
    value = c("Male", "Female"),
    stringsAsFactors = FALSE
  )

  result <- apply_lookup(df, SEX, SEXF, lookup_df)

  expect_equal(result$SEXF, c("Male", "Female", "Male"))
})

test_that("apply_lookup supports a custom unmatched value", {
  df <- data.frame(SEX = c("M", "U"), stringsAsFactors = FALSE)
  lookup <- c("M" = "Male", "F" = "Female")

  result <- apply_lookup(df, SEX, SEXF, lookup, unmatched = "UNKNOWN")

  expect_equal(result$SEXF, c("Male", "UNKNOWN"))
})

test_that("apply_lookup errors on unmatched values when unmatched = 'error'", {
  df <- data.frame(SEX = c("M", "U"), stringsAsFactors = FALSE)
  lookup <- c("M" = "Male", "F" = "Female")

  expect_error(
    apply_lookup(df, SEX, SEXF, lookup, unmatched = "error"),
    "no match in `lookup`"
  )
})

test_that("apply_lookup errors when var is not a column in dataset", {
  df <- data.frame(SEX = c("M"), stringsAsFactors = FALSE)
  lookup <- c("M" = "Male")

  expect_error(apply_lookup(df, GENDER, SEXF, lookup), "not a column in `dataset`")
})

test_that("apply_lookup errors on invalid lookup input", {
  df <- data.frame(SEX = c("M"), stringsAsFactors = FALSE)

  expect_error(apply_lookup(df, SEX, SEXF, lookup = c("Male", "Female")), "named vector")
})
