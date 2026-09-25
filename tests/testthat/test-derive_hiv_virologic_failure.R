test_that("derive_hiv_virologic_failure derives configured suspected and confirmed flags", {
  dat <- data.frame(
    USUBJID = c("S2", "S1", "S1", "S2", "S1", "S3", "S4", "S3", "S4", "S3", "S4"),
    ADT = as.Date(c(
      "2021-04-30", "2021-01-01", "2021-01-20", "2021-05-10", "2021-04-30",
      "2021-01-01", "2021-01-01", "2021-01-30", "2021-01-10", "2021-02-10",
      "2021-02-20"
    )),
    ADY = c(120, 1, 20, 130, 120, 1, 1, 30, 10, 40, 60),
    AVAL = c(60, 20, 25, 55, 100, 1000, 100, 400, 80, 500, 2500),
    L10AVAL = log10(c(60, 20, 25, 55, 100, 1000, 100, 400, 80, 500, 2500)),
    L10CHG = c(NA, NA, NA, NA, NA, 0, NA, -0.4, NA, -0.3, NA),
    ABLFL = c(NA, "Y", NA, NA, NA, "Y", NA, NA, NA, NA, NA),
    REFL = c(NA, NA, NA, NA, NA, NA, NA, "Y", NA, NA, NA),
    SEQ = c(1, 1, 2, 2, 3, 1, 1, 2, 2, 3, 3)
  )
  criteria <- data.frame(
    criterion = c("no_supp", "rebound", "nadir", "early"),
    type = c(
      "no_prior_confirmed_suppression",
      "rebound_after_confirmed_suppression",
      "above_nadir_increment",
      "early_inadequate_response"
    ),
    threshold = c(50, 50, 50, 50),
    post_baseline_start_day = c(100, 1, 2, 2),
    suppression_start_day = c(1, 1, 2, 2),
    early_response_end_day = c(NA_real_, NA_real_, NA_real_, 45),
    min_confirmation_interval = c(7, 7, 7, 7),
    log_nadir_increment = c(NA_real_, NA_real_, 1, 1),
    require_resistance = c(FALSE, FALSE, FALSE, TRUE),
    suspected_flag = c("NS_SFL", "RB_SFL", "ND_SFL", "ER_SFL"),
    confirmed_flag = c("NS_CFL", "RB_CFL", "ND_CFL", "ER_CFL")
  )

  result <- derive_hiv_virologic_failure(
    dat,
    criteria = criteria,
    log_value_var = "L10AVAL",
    log_change_var = "L10CHG",
    baseline_flag_var = "ABLFL",
    resistance_var = "REFL",
    tie_break_vars = "SEQ"
  )

  expect_equal(result$USUBJID, dat$USUBJID)
  expect_equal(result$NS_SFL[dat$USUBJID == "S2" & dat$ADY == 120], "Y")
  expect_equal(result$NS_CFL[dat$USUBJID == "S2" & dat$ADY == 120], "Y")
  expect_equal(result$NS_SFL[dat$USUBJID == "S1" & dat$ADY == 120], NA_character_)
  expect_equal(result$RB_SFL[dat$USUBJID == "S1" & dat$ADY == 120], "Y")
  expect_equal(result$RB_CFL[dat$USUBJID == "S1" & dat$ADY == 120], NA_character_)
  expect_equal(result$ND_SFL[dat$USUBJID == "S4" & dat$ADY == 60], "Y")
  expect_equal(result$ER_SFL[dat$USUBJID == "S3" & dat$ADY == 30], "Y")
  expect_equal(result$ER_CFL[dat$USUBJID == "S3" & dat$ADY == 30], "Y")
  expect_equal(result$HIVVFSPFL[dat$USUBJID == "S3" & dat$ADY == 1], NA_character_)
})

test_that("derive_hiv_virologic_failure requires explicit tie breaks for duplicate dates", {
  dat <- data.frame(
    USUBJID = c("S1", "S1"),
    ADT = as.Date(c("2021-01-01", "2021-01-01")),
    ADY = c(1, 1),
    AVAL = c(20, 30)
  )
  criteria <- data.frame(
    criterion = "vf",
    type = "no_prior_confirmed_suppression",
    threshold = 50,
    post_baseline_start_day = 1,
    min_confirmation_interval = 7
  )

  expect_error(
    derive_hiv_virologic_failure(dat, criteria),
    "explicit `tie_break_vars`"
  )
})

test_that("derive_hiv_virologic_failure validates resistance requirements", {
  dat <- data.frame(
    USUBJID = "S1",
    ADT = as.Date("2021-01-10"),
    ADY = 10,
    AVAL = 100,
    L10CHG = -0.2,
    ABLFL = NA_character_
  )
  criteria <- data.frame(
    criterion = "early",
    type = "early_inadequate_response",
    threshold = 50,
    post_baseline_start_day = 2,
    early_response_end_day = 45,
    min_confirmation_interval = 7,
    log_nadir_increment = 1,
    require_resistance = TRUE
  )

  expect_error(
    derive_hiv_virologic_failure(
      dat,
      criteria,
      log_change_var = "L10CHG",
      baseline_flag_var = "ABLFL"
    ),
    "`resistance_var` is required"
  )
})
