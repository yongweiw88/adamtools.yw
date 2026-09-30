test_that("derive_avisit_from_visitnum maps scheduled and unscheduled visits", {
  dat <- data.frame(VISITNUM = c(1, 2.5, NA_real_))
  labels <- data.frame(VISITNUM = 1, AVISIT = "Baseline")

  result <- derive_avisit_from_visitnum(dat, VISITNUM, labels)

  expect_equal(result$AVISIT, c("Baseline", "UNSCHEDULED", NA))
  expect_equal(result$AVISITN, c(1, 900, NA))
})

test_that("derive_bidirectional_tox_grade separates low and high grades", {
  dat <- data.frame(
    AVAL = c(2, 12, 5, NA),
    ANRLO = c(4, 4, 4, 4),
    ANRHI = c(10, 10, 10, 10),
    ATOXGR = c("GRADE 2", "GRADE 3", "GRADE 0", NA_character_),
    ATOXGRN = c(2, 3, 0, NA_real_)
  )

  result <- derive_bidirectional_tox_grade(dat)

  expect_equal(result$ATOXGRL, c("GRADE 2", "", "", ""))
  expect_equal(result$ATOXGRLN, c(2, NA, NA, NA))
  expect_equal(result$ATOXGRH, c("", "GRADE 3", "", ""))
  expect_equal(result$ATOXGRHN, c(NA, 3, NA, NA))
})

test_that("derive_disposition_triplet creates status, reason, and subreason rows", {
  dat <- data.frame(
    STUDYID = "S",
    USUBJID = c("1", "2"),
    DSSEQ = 1:2,
    DSTERM = c("COMPLETED", "WITHDRAWN: SUBJECT DECISION"),
    DSDECOD = c("COMPLETED", "WITHDRAWN"),
    DSCAT = "DISPOSITION EVENT",
    DSSCAT = "STUDY",
    DSSTDTC = c("2024-01-01", "2024-02-01"),
    DSTERM1 = c(NA_character_, NA_character_)
  )

  result <- derive_disposition_triplet(
    dat,
    "STAT", "Status", 1, "WRES", "Reason", 2, "WSRES", "Subreason", 3,
    "Study", 1
  )

  expect_identical(as.integer(table(result$PARAMCD)), c(2L, 1L, 1L))
  subreason <- result[result$PARAMCD == "WSRES", ]
  expect_equal(subreason$AVALC, "SUBJECT DECISION")
  expect_equal(subreason$SRCVAR, "DSTERM")
})

test_that("derive_group_max_flag flags every maximum and returns the maximum", {
  dat <- data.frame(
    USUBJID = c("1", "1", "1", "2"),
    ATOXGRN = c(2, 3, 3, NA_real_)
  )

  result <- derive_group_max_flag(
    dat, by_vars = "USUBJID", value_var = "ATOXGRN",
    flag_var = "MAXFL", max_value_var = "MAXGR"
  )

  expect_equal(result$MAXFL, c("", "Y", "Y", ""))
  expect_equal(result$MAXGR, c(3, 3, 3, NA))
})

test_that("derive_keyword_flag searches across text variables and missing values", {
  dat <- data.frame(A = c("History of depression", NA, "Other"), B = c(NA, "ANXIETY", "other"))

  result <- derive_keyword_flag(dat, c("A", "B"), "anxiety", "MHANXFL")

  expect_equal(result$MHANXFL, c("", "Y", ""))
})

test_that("derive_mixture_expansion retains originals and emits alternatives", {
  dat <- data.frame(AVALC = c("V381V/I", "K103N"), USUBJID = c("1", "2"))

  result <- derive_mixture_expansion(dat, "AVALC")

  expect_equal(nrow(result), 4L)
  expect_setequal(result$AVALC, c("V381V/I", "V381V", "V381I", "K103N"))
})

test_that("derive_ordinal_grade_worst_case_records derives independent tracks", {
  dat <- data.frame(
    USUBJID = c("1", "1", "1"),
    PARAMCD = "TEST",
    AVALCN = c(1, 3, 2),
    ATOXGRN = c(1, 1, 3),
    ADT = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))
  )

  result <- derive_ordinal_grade_worst_case_records(
    dat, by_vars = c("USUBJID", "PARAMCD"),
    ordinal_var = "AVALCN", ordinal_dtype = "WOC",
    grade_var = "ATOXGRN", grade_dtype = "WOCGR",
    tie_break = "ADT"
  )

  expect_equal(result$DTYPE, c("WOC", "WOCGR"))
  expect_equal(result$AVALCN, c(3, 2))
  expect_equal(result$ATOXGRN, c(1, 3))
})

test_that("derive_paired_event_subjects finds groups with nearby event dates", {
  event1 <- data.frame(
    USUBJID = c("1", "2"),
    ADT = as.Date(c("2024-01-01", "2024-01-01"))
  )
  event2 <- data.frame(
    USUBJID = c("1", "2"),
    ADT = as.Date(c("2024-01-29", "2024-01-30"))
  )

  result <- derive_paired_event_subjects(event1, event2, within_days = 28)

  expect_equal(result$USUBJID, "1")
})

test_that("derive_paramcd_from_lookup supports compound keys and blank matching", {
  dat <- data.frame(TESTCD = c("SYSBP", "SYSBP"), POS = c("", "SITTING"))
  lookup <- data.frame(TESTCD = c("SYSBP", "SYSBP"), POS = c(NA, "SITTING"), PARAMCD = c("SYSBP", "SYSBPS"))

  result <- derive_paramcd_from_lookup(
    dat, lookup, by = c("TESTCD", "POS"), value_var = "PARAMCD",
    fallback_var = "TESTCD", quiet = TRUE
  )

  expect_equal(result$PARAMCD, c("SYSBP", "SYSBPS"))
  duplicate_lookup <- rbind(lookup, transform(lookup[1, ], PARAMCD = "OTHER"))
  expect_error(
    derive_paramcd_from_lookup(dat, duplicate_lookup, by = c("TESTCD", "POS"),
                               value_var = "PARAMCD", quiet = TRUE),
    "duplicate rows"
  )
})

test_that("derive_range_indicator classifies values and missing limits", {
  dat <- data.frame(AVAL = c(1, 11, 5, 5, NA), ANRLO = c(2, 2, 2, NA, 2), ANRHI = c(10, 10, 10, 10, 10))

  result <- derive_range_indicator(dat)

  expect_equal(result$ANRIND, c("LOW", "HIGH", "NORMAL", "NORMAL", NA))
})

test_that("derive_replicate_average_record emits group means", {
  dat <- data.frame(
    USUBJID = c("1", "1", "2"),
    PARAMCD = "QT",
    AVAL = c(400, 420, 410),
    ADT = as.Date(c("2024-01-01", "2024-01-01", "2024-01-02")),
    SEQ = c(1, 2, 1)
  )

  result <- derive_replicate_average_record(
    dat, by_vars = c("USUBJID", "PARAMCD"), dtype = "AVERAGE",
    order_vars = c("ADT", "SEQ")
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$USUBJID, "1")
  expect_equal(result$AVAL, 410)
  expect_equal(result$DTYPE, "AVERAGE")
})

test_that("derive_worst_case_direction_grade defaults evaluable missing directions to zero", {
  dat <- data.frame(ATOXGRN = c(0, 2, NA), ATOXGRLN = c(NA, 1, NA), ATOXGRHN = c(NA, NA, NA))

  result <- derive_worst_case_direction_grade(dat)

  expect_equal(result$WC_ATOXGRLN, c(0, 1, NA))
  expect_equal(result$WC_ATOXGRHN, c(0, 0, NA))
})

test_that("format_duration_dhm formats minutes and preserves missingness", {
  expect_equal(
    format_duration_dhm(c(1501, 60, NA_real_)),
    c("1d 1h 1m", "0d 1h 0m", NA_character_)
  )
})
