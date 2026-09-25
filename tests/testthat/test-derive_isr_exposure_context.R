test_that("derive_isr_exposure_context selects prior exposure by category mapping", {
  ae <- data.frame(
    USUBJID = c("01", "01", "01", "02"),
    AECAT = c("AE-A", "AE-A", "AE-A", "AE-B"),
    ASTDT = as.Date(c("2024-01-10", "2024-01-05", NA, "2024-01-06")),
    AESEQ = 1:4,
    stringsAsFactors = FALSE
  )
  ex <- data.frame(
    USUBJID = c("01", "01", "01", "02"),
    EXCAT = c("EX-X", "EX-X", "EX-X", "EX-Y"),
    EXDT = as.Date(c("2024-01-01", "2024-01-08", "2024-01-12", "2024-01-07")),
    VISIT = c("Dose 1", "Dose 2", "Future Dose", "Future Other"),
    stringsAsFactors = FALSE
  )
  cat_map <- data.frame(
    ae_category = c("AE-A", "AE-B"),
    exposure_category = c("EX-X", "EX-Y"),
    stringsAsFactors = FALSE
  )

  result <- derive_isr_exposure_context(
    ae_dataset = ae,
    exposure_dataset = ex,
    by_vars = "USUBJID",
    ae_date_var = "ASTDT",
    exposure_date_var = "EXDT",
    ae_category_var = "AECAT",
    exposure_category_var = "EXCAT",
    category_map = cat_map,
    first_exposure_date_var = "FIRSTDT",
    latest_exposure_date_var = "LASTDT",
    on_or_after_first_flag_var = "ONTRTFL",
    latest_exposure_vars = c(LASTVIS = "VISIT")
  )

  expect_equal(nrow(result), nrow(ae))
  expect_equal(result$AESEQ, ae$AESEQ)
  expect_equal(result$FIRSTDT[1], as.Date("2024-01-01"))
  expect_equal(result$LASTDT[1], as.Date("2024-01-08"))
  expect_equal(result$LASTVIS[1], "Dose 2")
  expect_equal(result$FIRSTDT[2], as.Date("2024-01-01"))
  expect_equal(result$LASTDT[2], as.Date("2024-01-01"))
  expect_equal(result$ONTRTFL[1:2], c("Y", "Y"))
  expect_true(is.na(result$FIRSTDT[3]))
  expect_true(is.na(result$LASTDT[3]))
  expect_equal(result$ONTRTFL[3], "N")
  expect_true(is.na(result$FIRSTDT[4]))
  expect_true(is.na(result$LASTDT[4]))
  expect_equal(result$ONTRTFL[4], "N")
})

test_that("derive_isr_exposure_context excludes future exposure and validates duplicates", {
  ae <- data.frame(
    USUBJID = "01",
    AECAT = "A",
    ASTDT = as.Date("2024-01-10"),
    stringsAsFactors = FALSE
  )
  ex <- data.frame(
    USUBJID = c("01", "01"),
    EXCAT = c("A", "A"),
    EXDT = as.Date(c("2024-01-12", "2024-01-12")),
    VISIT = c("Future 1", "Future 2"),
    stringsAsFactors = FALSE
  )

  no_prior <- derive_isr_exposure_context(
    ae_dataset = ae,
    exposure_dataset = ex[1, , drop = FALSE],
    by_vars = "USUBJID",
    ae_date_var = "ASTDT",
    exposure_date_var = "EXDT",
    ae_category_var = "AECAT",
    exposure_category_var = "EXCAT",
    first_exposure_date_var = "FIRSTDT",
    latest_exposure_date_var = "LASTDT",
    on_or_after_first_flag_var = "ONTRTFL"
  )
  expect_true(is.na(no_prior$LASTDT[1]))
  expect_equal(no_prior$ONTRTFL[1], "N")

  expect_error(
    derive_isr_exposure_context(
      ae_dataset = ae,
      exposure_dataset = ex,
      by_vars = "USUBJID",
      ae_date_var = "ASTDT",
      exposure_date_var = "EXDT",
      ae_category_var = "AECAT",
      exposure_category_var = "EXCAT",
      first_exposure_date_var = "FIRSTDT",
      latest_exposure_date_var = "LASTDT",
      on_or_after_first_flag_var = "ONTRTFL",
      latest_exposure_vars = c(LASTVIS = "VISIT")
    ),
    "duplicate exposure records"
  )

  exact_dups <- ex[c(1, 1), , drop = FALSE]
  distinct_result <- derive_isr_exposure_context(
    ae_dataset = ae,
    exposure_dataset = exact_dups,
    by_vars = "USUBJID",
    ae_date_var = "ASTDT",
    exposure_date_var = "EXDT",
    ae_category_var = "AECAT",
    exposure_category_var = "EXCAT",
    first_exposure_date_var = "FIRSTDT",
    latest_exposure_date_var = "LASTDT",
    on_or_after_first_flag_var = "ONTRTFL",
    latest_exposure_vars = c(LASTVIS = "VISIT"),
    duplicate_exposure_action = "distinct"
  )
  expect_true(is.na(distinct_result$LASTDT[1]))
  expect_equal(distinct_result$ONTRTFL[1], "N")
})
