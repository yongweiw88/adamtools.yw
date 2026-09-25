test_that("derive_analysis_visit_window maps day to visit window", {
  window_map <- data.frame(
    AWLO = c(1, 15, 43),
    AWHI = c(14, 42, 999),
    AVISIT = c("Day 1", "Day 2 - Day 14", "Day 15 - End"),
    AVISITN = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  df <- data.frame(USUBJID = c("1", "2", "3", "4"), ADY = c(1, 20, 43, NA))

  result <- derive_analysis_visit_window(df, ADY, window_map)

  expect_equal(result$AVISIT, c("Day 1", "Day 2 - Day 14", "Day 15 - End", NA))
  expect_equal(result$AVISITN, c(1, 2, 3, NA))
})

test_that("derive_analysis_visit_window derives AWTARGET and AWRANGE", {
  window_map <- data.frame(
    AWLO = c(1, 15),
    AWHI = c(14, 42),
    AVISIT = c("Day 1", "Day 2"),
    AVISITN = c(1, 2),
    AWTARGET = c(1, 28),
    stringsAsFactors = FALSE
  )
  df <- data.frame(ADY = c(1, 20))

  result <- derive_analysis_visit_window(
    df, ADY, window_map,
    derive_awtarget = TRUE, derive_awrange = TRUE
  )

  expect_equal(result$AWTARGET, c(1, 28))
  expect_equal(result$AWRANGE, c("1 to 14", "15 to 42"))
})

test_that("derive_analysis_visit_window errors on unmatched when requested", {
  window_map <- data.frame(AWLO = 1, AWHI = 5, AVISIT = "Day 1", AVISITN = 1)
  df <- data.frame(ADY = c(1, 100))

  expect_error(
    derive_analysis_visit_window(df, ADY, window_map, unmatched = "error"),
    "did not fall within any window"
  )
  expect_warning(
    derive_analysis_visit_window(df, ADY, window_map, unmatched = "warn"),
    "did not fall within any window"
  )
})
