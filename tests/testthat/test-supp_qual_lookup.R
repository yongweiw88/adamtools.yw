test_that("supp_qual_lookup pivots selected QNAM values to wide", {
  supp <- data.frame(
    STUDYID = "S1",
    USUBJID = c("1", "1", "2"),
    IDVAR = "AESEQ",
    IDVARVAL = c("1", "1", "2"),
    QNAM = c("COMMENT", "SEVFL", "COMMENT"),
    QVAL = c("note1", "Y", "note2"),
    stringsAsFactors = FALSE
  )

  result <- supp_qual_lookup(supp, idvar_to = "AESEQ")

  expect_true(all(c("STUDYID", "USUBJID", "AESEQ", "COMMENT", "SEVFL") %in% names(result)))
  expect_equal(result$AESEQ, c(1, 2))
  expect_equal(sort(result$COMMENT), c("note1", "note2"))
})

test_that("supp_qual_lookup filters to selected qnam", {
  supp <- data.frame(
    USUBJID = c("1", "1"),
    IDVARVAL = c("1", "1"),
    QNAM = c("COMMENT", "SEVFL"),
    QVAL = c("note1", "Y"),
    stringsAsFactors = FALSE
  )

  result <- supp_qual_lookup(supp, qnam = "COMMENT", keys = "USUBJID")
  expect_true("COMMENT" %in% names(result))
  expect_false("SEVFL" %in% names(result))
})

test_that("supp_qual_lookup errors on missing required columns", {
  expect_error(supp_qual_lookup(data.frame(x = 1)), "missing required column")
})
