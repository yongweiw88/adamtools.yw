test_that("derive_pediatric_lms_zscore applies LMS and log fallback preserving rows", {
  measurements <- data.frame(
    ROWID = c("third", "first", "second"),
    SEX = c("F", "F", "M"),
    TYPE = c("WEIGHT", "WEIGHT", "HEIGHT"),
    COORD = c(10, 20, 30),
    AVAL = c(15, 10 * exp(0.4), 12),
    stringsAsFactors = FALSE
  )
  reference <- data.frame(
    RSEX = c("F", "F", "M"),
    RTYPE = c("WEIGHT", "WEIGHT", "HEIGHT"),
    RCOORD = c(10, 20, 30),
    L = c(1, 0, 1),
    M = c(10, 10, 10),
    S = c(0.5, 0.2, 0.2),
    stringsAsFactors = FALSE
  )

  result <- derive_pediatric_lms_zscore(
    measurements,
    reference,
    value_var = "AVAL",
    sex_var = "SEX",
    coordinate_var = "COORD",
    by_vars = "TYPE",
    ref_sex_var = "RSEX",
    ref_coordinate_var = "RCOORD",
    ref_by_vars = "RTYPE",
    z_var = "Z",
    diagnostics = TRUE,
    match_flag_var = "MATCHFL",
    ref_coordinate_output_var = "REFCOORD"
  )

  expect_equal(result$ROWID, measurements$ROWID)
  expect_equal(result$Z, c(1, 2, 1), tolerance = 1e-12)
  expect_equal(result$MATCHFL, c("Y", "Y", "Y"))
  expect_equal(result$REFCOORD, measurements$COORD)
})

test_that("derive_pediatric_lms_zscore supports documented WHO tail extension", {
  measurements <- data.frame(
    SEX = "F",
    COORD = 1,
    AVAL = 10 * (1 + 0.5 * 0.1 * 4)^2,
    stringsAsFactors = FALSE
  )
  reference <- data.frame(
    SEX = "F",
    COORD = 1,
    L = 0.5,
    M = 10,
    S = 0.1,
    stringsAsFactors = FALSE
  )

  result <- derive_pediatric_lms_zscore(
    measurements,
    reference,
    value_var = "AVAL",
    sex_var = "SEX",
    coordinate_var = "COORD",
    z_var = "Z",
    method = "who_extended"
  )

  sd2 <- 10 * (1 + 0.5 * 0.1 * 2)^2
  sd3 <- 10 * (1 + 0.5 * 0.1 * 3)^2
  expected <- 3 + (measurements$AVAL - sd3) / (sd3 - sd2)
  expect_equal(result$Z, expected, tolerance = 1e-12)
  expect_gt(result$Z, 4)
})

test_that("derive_pediatric_lms_zscore handles unmatched rows by policy and duplicate reference errors", {
  measurements <- data.frame(
    SEX = c("F", "M"),
    COORD = c(1, 2),
    AVAL = c(10, 20),
    stringsAsFactors = FALSE
  )
  reference <- data.frame(
    SEX = "F",
    COORD = 1,
    L = 1,
    M = 10,
    S = 0.2,
    stringsAsFactors = FALSE
  )

  expect_error(
    derive_pediatric_lms_zscore(
      measurements,
      reference,
      value_var = "AVAL",
      sex_var = "SEX",
      coordinate_var = "COORD",
      z_var = "Z"
    ),
    "no matching LMS reference row"
  )

  result <- derive_pediatric_lms_zscore(
    measurements,
    reference,
    value_var = "AVAL",
    sex_var = "SEX",
    coordinate_var = "COORD",
    z_var = "Z",
    unmatched = "NA",
    diagnostics = TRUE,
    match_flag_var = "MATCHFL"
  )
  expect_equal(result$MATCHFL, c("Y", "N"))
  expect_true(is.na(result$Z[2]))

  duplicate_reference <- rbind(reference, reference)
  expect_error(
    derive_pediatric_lms_zscore(
      measurements[1, , drop = FALSE],
      duplicate_reference,
      value_var = "AVAL",
      sex_var = "SEX",
      coordinate_var = "COORD",
      z_var = "Z"
    ),
    "duplicate LMS rows"
  )
})

test_that("derive_pediatric_lms_zscore fails on invalid measurement and reference values", {
  measurements <- data.frame(
    SEX = "F",
    COORD = 1,
    AVAL = 0,
    stringsAsFactors = FALSE
  )
  reference <- data.frame(
    SEX = "F",
    COORD = 1,
    L = 1,
    M = 10,
    S = 0.2,
    stringsAsFactors = FALSE
  )

  expect_error(
    derive_pediatric_lms_zscore(
      measurements,
      reference,
      value_var = "AVAL",
      sex_var = "SEX",
      coordinate_var = "COORD",
      z_var = "Z"
    ),
    "invalid"
  )

  bad_reference <- reference
  bad_reference$S <- 0
  measurements$AVAL <- 10
  expect_error(
    derive_pediatric_lms_zscore(
      measurements,
      bad_reference,
      value_var = "AVAL",
      sex_var = "SEX",
      coordinate_var = "COORD",
      z_var = "Z"
    ),
    "invalid"
  )
})
