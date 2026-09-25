test_that("derive_param_lookup_from_reference builds a distinct lookup", {
  ref <- data.frame(
    TESTCD = c("WEIGHT", "WEIGHT", "HEIGHT"),
    PARAMCD = c("WEIGHTBL", "WEIGHTBL", "HEIGHTBL"),
    PARAM = c("Weight (kg)", "Weight (kg)", "Height (cm)"),
    PARAMN = c(1, 1, 2),
    stringsAsFactors = FALSE
  )

  result <- derive_param_lookup_from_reference(ref)

  expect_equal(nrow(result), 2)
  expect_true(all(c("TESTCD", "PARAMCD", "PARAM", "PARAMN") %in% names(result)))
})

test_that("derive_param_lookup_from_reference supports keep_paramcd and extra_keys", {
  ref <- data.frame(
    LBTESTCD = c("ALB", "ALT", "ALB"),
    LBSPEC = c("SERUM", "SERUM", "URINE"),
    PARAMCD = c("ALB", "ALT", "ALBU"),
    PARAM = c("Albumin (Serum)", "ALT", "Albumin (Urine)"),
    stringsAsFactors = FALSE
  )

  result <- derive_param_lookup_from_reference(
    ref,
    testcd_var = "LBTESTCD",
    keep_paramcd = c("ALB", "ALBU"),
    extra_keys = "LBSPEC"
  )

  expect_equal(nrow(result), 2)
  expect_true("LBSPEC" %in% names(result))
  expect_false("ALT" %in% result$PARAMCD)
})

test_that("derive_param_lookup_from_reference errors on missing columns", {
  expect_error(derive_param_lookup_from_reference(data.frame(x = 1)), "missing required column")
})
