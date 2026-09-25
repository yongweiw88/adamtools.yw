test_that("derive_worst_case_grade_records selects worst absolute grade per group", {
  dat <- data.frame(
    USUBJID = c("1", "1", "1", "2", "2"),
    PARAMCD = c("ALT", "ALT", "ALT", "ALT", "ALT"),
    ADT = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-01-01", "2020-02-01")),
    LBSEQ = c(1, 2, 3, 1, 2),
    ATOXGR = c(1, -3, 2, -1, 1),
    stringsAsFactors = FALSE
  )

  result <- derive_worst_case_grade_records(
    dat,
    by_vars = c("USUBJID", "PARAMCD"),
    grade_var = "ATOXGR",
    dtype = "WOC",
    direction = "abs",
    tie_break = c("ADT", "LBSEQ")
  )

  expect_equal(nrow(result), 2L)
  expect_equal(result$DTYPE, c("WOC", "WOC"))
  expect_equal(result$ATOXGR[result$USUBJID == "1"], -3)
  # Subject 2's two records tie on abs(grade) == 1; the tie-break (later
  # ADT/LBSEQ) resolves to the more recent record.
  expect_equal(result$ATOXGR[result$USUBJID == "2"], 1)
})

test_that("derive_worst_case_grade_records supports high and low direction", {
  dat <- data.frame(
    USUBJID = c("1", "1", "1"),
    PARAMCD = c("ALT", "ALT", "ALT"),
    ADT = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    LBSEQ = c(1, 2, 3),
    ATOXGR = c(1, -3, 2),
    stringsAsFactors = FALSE
  )

  high <- derive_worst_case_grade_records(
    dat,
    by_vars = c("USUBJID", "PARAMCD"),
    grade_var = "ATOXGR",
    dtype = "WOCGRH",
    direction = "high",
    tie_break = c("ADT", "LBSEQ")
  )
  low <- derive_worst_case_grade_records(
    dat,
    by_vars = c("USUBJID", "PARAMCD"),
    grade_var = "ATOXGR",
    dtype = "WOCGRL",
    direction = "low",
    tie_break = c("ADT", "LBSEQ")
  )

  expect_equal(high$ATOXGR, 2)
  expect_equal(high$DTYPE, "WOCGRH")
  expect_equal(low$ATOXGR, -3)
  expect_equal(low$DTYPE, "WOCGRL")
})

test_that("derive_worst_case_grade_records respects the eligibility condition", {
  dat <- data.frame(
    USUBJID = c("1", "1"),
    PARAMCD = c("ALT", "ALT"),
    ADT = as.Date(c("2020-01-01", "2020-02-01")),
    LBSEQ = c(1, 2),
    ATOXGR = c(3, 1),
    ONTRTFL = c("N", "Y"),
    stringsAsFactors = FALSE
  )

  result <- derive_worst_case_grade_records(
    dat,
    by_vars = c("USUBJID", "PARAMCD"),
    grade_var = "ATOXGR",
    dtype = "WOC",
    condition = dat$ONTRTFL == "Y",
    direction = "abs",
    tie_break = c("ADT", "LBSEQ")
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$ATOXGR, 1)
})

test_that("derive_worst_case_grade_records returns zero rows when nothing qualifies", {
  dat <- data.frame(
    USUBJID = c("1", "1"),
    PARAMCD = c("ALT", "ALT"),
    ADT = as.Date(c("2020-01-01", "2020-02-01")),
    LBSEQ = c(1, 2),
    ATOXGR = c(NA_real_, NA_real_),
    stringsAsFactors = FALSE
  )

  result <- derive_worst_case_grade_records(
    dat,
    by_vars = c("USUBJID", "PARAMCD"),
    grade_var = "ATOXGR",
    dtype = "WOC",
    direction = "abs",
    tie_break = c("ADT", "LBSEQ")
  )

  expect_equal(nrow(result), 0L)
  expect_true("DTYPE" %in% names(result))
})

test_that("derive_worst_case_grade_records errors on unresolved ties", {
  dat <- data.frame(
    USUBJID = c("1", "1"),
    PARAMCD = c("ALT", "ALT"),
    ATOXGR = c(3, 3),
    stringsAsFactors = FALSE
  )

  expect_error(
    derive_worst_case_grade_records(
      dat,
      by_vars = c("USUBJID", "PARAMCD"),
      grade_var = "ATOXGR",
      dtype = "WOC",
      direction = "abs"
    ),
    "Non-unique selected record"
  )
})
