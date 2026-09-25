test_that("dtc_date_imputation_flag identifies partial date precision", {
  flags <- dtc_date_imputation_flag(c(
    "2020",
    "2020-05",
    "2020-05-07",
    "2020-05-07T08:09",
    "",
    NA,
    "2020-13"
  ))

  expect_equal(flags, c("M", "D", "", "", "", "", ""))
})

test_that("derive_dtc_date_vars derives dates and ADaM date flags", {
  dat <- data.frame(
    USUBJID = c("1", "2", "3", "4"),
    BRTHDTC = c("2020", "2020-05", "2020-05-07", ""),
    stringsAsFactors = FALSE
  )

  result <- derive_dtc_date_vars(
    dat,
    mappings = c(BRTHDTC = "BRTHDT"),
    flag_mappings = c(BRTHDTC = "BRTHDTF"),
    highest_imputation = "M",
    date_imputation = "last"
  )

  expect_equal(result$BRTHDT, as.Date(c("2020-12-31", "2020-05-31", "2020-05-07", NA)))
  expect_equal(result$BRTHDTF, c("M", "D", "", ""))
})

test_that("derive_dtc_datetime_vars derives datetimes and clock times", {
  dat <- data.frame(
    USUBJID = c("1", "2", "3"),
    EXSTDTC = c("2020-05-07T08:09", "2020-05-08T11:12:13", "2020-05-09"),
    stringsAsFactors = FALSE
  )

  result <- derive_dtc_datetime_vars(
    dat,
    mappings = c(EXSTDTC = "EXSTDTM"),
    time_mappings = TRUE
  )

  expect_s3_class(result$EXSTDTM, "POSIXct")
  expect_s3_class(result$EXSTTM, "hms")
  expect_equal(as.character(result$EXSTTM), c("08:09:00", "11:12:13", NA))
  expect_true(is.na(result$EXSTDTM[3]))
})

test_that("derive_dtc helpers protect existing output columns", {
  dat <- data.frame(DTC = "2020-01-01", ADT = as.Date("2020-01-01"), stringsAsFactors = FALSE)
  expect_error(
    derive_dtc_date_vars(dat, mappings = c(DTC = "ADT")),
    "already exist"
  )
})
