test_that("derive_age_groups creates auto-generated labels", {
  dat <- data.frame(AGE = c(17, 18, 64, 65, 80))

  result <- derive_age_groups(dat, cutpoints = c(18, 65))

  expect_equal(result$AGEGR1, c("<18", "18-64", "18-64", ">=65", ">=65"))
  expect_equal(result$AGEGR1N, c(1, 2, 2, 3, 3))
})

test_that("derive_age_groups supports custom labels", {
  dat <- data.frame(AGE = c(10, 50))
  result <- derive_age_groups(dat, cutpoints = 40, labels = c("Young", "Old"))
  expect_equal(result$AGEGR1, c("Young", "Old"))
})

test_that("derive_age_groups errors on mismatched label length", {
  dat <- data.frame(AGE = 10)
  expect_error(derive_age_groups(dat, cutpoints = c(18, 65), labels = c("a", "b")), "labels")
})
