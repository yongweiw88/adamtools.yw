test_that("derive_race_groups collapses multi-race to MULTIPLE by default", {
  dat <- data.frame(RACE = c("WHITE", "WHITE,BLACK OR AFRICAN AMERICAN", "ASIAN"), stringsAsFactors = FALSE)

  result <- derive_race_groups(dat)

  expect_equal(result$ARACE, c("WHITE", "MULTIPLE", "ASIAN"))
  expect_true(is.numeric(result$ARACEN))
})

test_that("derive_race_groups first-value handling and racemap", {
  dat <- data.frame(RACE = c("WHITE/BLACK OR AFRICAN AMERICAN", "ASIAN"), stringsAsFactors = FALSE)
  racemap <- c(WHITE = "White", ASIAN = "Asian")

  result <- derive_race_groups(dat, multiple_handling = "first", racemap = racemap)

  expect_equal(result$ARACE, c("WHITE", "ASIAN"))
  expect_equal(result$RACEGR1, c("White", "Asian"))
})

test_that("derive_race_groups errors when race_var missing", {
  expect_error(derive_race_groups(data.frame(x = 1)), "race_var")
})
