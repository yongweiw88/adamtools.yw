test_that("strip_var_attrs removes label and format.sas attributes", {
  df <- data.frame(x = 1:3)
  attr(df$x, "label") <- "X Label"
  attr(df$x, "format.sas") <- "8."

  result <- strip_var_attrs(df)
  expect_null(attr(result$x, "label"))
  expect_null(attr(result$x, "format.sas"))
  expect_equal(result$x, 1:3)
})

test_that("strip_var_attrs only strips selected vars", {
  df <- data.frame(x = 1, y = 2)
  attr(df$x, "label") <- "X"
  attr(df$y, "label") <- "Y"

  result <- strip_var_attrs(df, vars = "x")
  expect_null(attr(result$x, "label"))
  expect_equal(attr(result$y, "label"), "Y")
})

test_that("strip_var_attrs errors on unknown vars", {
  df <- data.frame(x = 1)
  expect_error(strip_var_attrs(df, vars = "z"), "not in `dataset`")
})
