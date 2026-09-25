test_that("derive_bds_from_findings derives core BDS variables", {
  vs <- data.frame(
    STUDYID = "STUDY1",
    USUBJID = c("1", "1", "2"),
    VSTESTCD = c("WEIGHT", "WEIGHT", "WEIGHT"),
    VSTEST = c("Weight", "Weight", "Weight"),
    VSSTRESC = c("70", "72", "80"),
    VSSTRESN = c(70, 72, 80),
    VSDTC = c("2023-01-01", "2023-02-01", "2023-01-15"),
    VSBLFL = c("Y", "", "Y"),
    TRTSDT = as.Date(c("2023-01-01", "2023-01-01", "2023-01-15")),
    stringsAsFactors = FALSE
  )

  result <- derive_bds_from_findings(vs, domain = "VS")

  expect_true(all(c("ADT", "PARAMCD", "PARAM", "AVAL", "AVALC", "ADY", "BASE", "CHG", "PCHG") %in% names(result)))
  expect_equal(result$PARAMCD, c("WEIGHT", "WEIGHT", "WEIGHT"))
  expect_equal(result$AVAL, c(70, 72, 80))
  expect_equal(result$BASE[result$USUBJID == "1"], c(70, 70))
  expect_equal(result$CHG[2], 2)
})

test_that("derive_bds_from_findings applies param_map", {
  vs <- data.frame(
    STUDYID = "STUDY1",
    USUBJID = "1",
    VSTESTCD = "WEIGHT",
    VSTEST = "Weight",
    VSSTRESC = "70",
    VSSTRESN = 70,
    VSDTC = "2023-01-01",
    stringsAsFactors = FALSE
  )
  param_map <- data.frame(TESTCD = "WEIGHT", PARAMCD = "WEIGHTBL", PARAM = "Weight (kg)", stringsAsFactors = FALSE)

  result <- derive_bds_from_findings(vs, domain = "VS", param_map = param_map, derive_baseline = FALSE)

  expect_equal(result$PARAMCD, "WEIGHTBL")
  expect_equal(result$PARAM, "Weight (kg)")
})

test_that("derive_bds_from_findings errors on missing required variables", {
  expect_error(derive_bds_from_findings(data.frame(x = 1), domain = "VS"), "missing required variable")
})
