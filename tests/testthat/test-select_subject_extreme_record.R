test_that("select_subject_extreme_record selects last ordered record per subject", {
  dat <- data.frame(
    USUBJID = c("1", "1", "2", "2"),
    ADT = as.Date(c("2020-01-01", "2020-01-10", "2020-01-05", "2020-01-05")),
    SEQ = c(1, 2, 1, 2),
    AVAL = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )

  result <- select_subject_extreme_record(
    dat,
    order = c("ADT", "SEQ"),
    keep = c("ADT", "AVAL")
  )

  expect_equal(result$USUBJID, c("1", "2"))
  expect_equal(result$AVAL, c(20, 40))
})

test_that("select_subject_extreme_record can select first ordered record", {
  dat <- data.frame(
    USUBJID = c("1", "1"),
    ADT = as.Date(c("2020-01-10", "2020-01-01")),
    SEQ = c(2, 1),
    stringsAsFactors = FALSE
  )

  result <- select_subject_extreme_record(dat, order = c("ADT", "SEQ"), mode = "first")

  expect_equal(result$ADT, as.Date("2020-01-01"))
})

test_that("select_subject_extreme_record errors on selected ties by default", {
  dat <- data.frame(
    USUBJID = c("1", "1"),
    ADT = as.Date(c("2020-01-10", "2020-01-10")),
    SEQ = c(1, 1),
    AVAL = c(20, 30),
    stringsAsFactors = FALSE
  )

  expect_error(
    select_subject_extreme_record(dat, order = c("ADT", "SEQ")),
    "Non-unique selected record"
  )
})

test_that("select_subject_extreme_record supports explicit tie policy", {
  dat <- data.frame(
    USUBJID = c("1", "1"),
    ADT = as.Date(c("2020-01-10", "2020-01-10")),
    SEQ = c(1, 1),
    AVAL = c(20, 30),
    stringsAsFactors = FALSE
  )

  result <- select_subject_extreme_record(dat, order = c("ADT", "SEQ"), ties = "last")

  expect_equal(result$AVAL, 30)
})

test_that("select_subject_extreme_record rejects missing grouping keys", {
  dat <- data.frame(
    USUBJID = c("1", NA_character_),
    ADT = as.Date(c("2020-01-01", "2020-01-02")),
    stringsAsFactors = FALSE
  )

  expect_error(
    select_subject_extreme_record(dat, order = "ADT"),
    "grouping variables must not contain missing"
  )
})
