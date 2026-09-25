test_that("finalize_adam_dataset runs the metacore/xportr pipeline without erroring", {
  skip_if_not_installed("metacore")
  skip_if_not_installed("metatools")
  skip_if_not_installed("xportr")

  spec_path <- metacore::metacore_example("pilot_ADaM.rda")
  load(spec_path)
  ds_spec <- suppressWarnings(metacore::select_dataset(metacore, "ADSL"))

  adsl <- data.frame(
    STUDYID = "CDISCPILOT01",
    USUBJID = c("01-701-1015", "01-701-1023"),
    SUBJID = c("1015", "1023"),
    SITEID = c("701", "701"),
    ARM = c("Placebo", "Xanomeline High Dose"),
    TRT01P = c("Placebo", "Xanomeline High Dose"),
    stringsAsFactors = FALSE
  )

  result <- finalize_adam_dataset(
    adsl,
    domain = "ADSL",
    metacore = ds_spec,
    drop_unspec = TRUE,
    check = FALSE,
    order_cols = TRUE,
    apply_xportr_attrs = FALSE
  )

  expect_true(is.data.frame(result))
  expect_true(all(names(result) %in% c("STUDYID", "USUBJID", "SUBJID", "SITEID", "ARM", "TRT01P")))
})

test_that("finalize_adam_dataset errors when required args are missing", {
  df <- data.frame(x = 1)
  expect_error(finalize_adam_dataset(df, domain = NULL, metacore = list()), "`domain` is required")
  expect_error(finalize_adam_dataset(df, domain = "ADSL", metacore = NULL), "`metacore` is required")
  expect_error(finalize_adam_dataset(1, domain = "ADSL", metacore = list()), "must be a data frame")
})
