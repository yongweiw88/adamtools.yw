test_that("parse_hiv_resistance_mutations parses a simple substitution", {
  dat <- data.frame(GFSTRESC = "K103N", stringsAsFactors = FALSE)

  result <- parse_hiv_resistance_mutations(dat, result_var = GFSTRESC)

  expect_equal(nrow(result), 1L)
  expect_equal(result$REFRES, "K")
  expect_equal(result$GENLOC, "103")
  expect_equal(result$CODON, "K103")
  expect_equal(result$AAS, "N")
  expect_equal(result$MUTATION, "K103N")
  expect_true(is.na(result$RESCAT))
})

test_that("parse_hiv_resistance_mutations expands slash-separated alternatives to one row each", {
  dat <- data.frame(GFSTRESC = "T66A/I/K", stringsAsFactors = FALSE)

  result <- parse_hiv_resistance_mutations(dat, result_var = GFSTRESC)

  expect_equal(nrow(result), 3L)
  expect_equal(result$GENLOC, rep("66", 3L))
  expect_equal(result$AAS, c("A", "I", "K"))
  expect_equal(result$MUTATION, c("T66A", "T66I", "T66K"))
  expect_equal(result$.hiv_res_source_row, rep(1L, 3L))
})

test_that("parse_hiv_resistance_mutations handles insertion and deletion aliases", {
  dat <- data.frame(GFSTRESC = c("T69INS", "T69DEL"), stringsAsFactors = FALSE)

  result <- parse_hiv_resistance_mutations(dat, result_var = GFSTRESC)

  expect_equal(result$RESCAT, c("INSERTION", "DELETION"))
  expect_true(is.na(result$AAS[[1]]))
  expect_equal(result$MUTATION, c("T69INS", "T69DEL"))
})

test_that("parse_hiv_resistance_mutations skips missing values by default and can keep them", {
  dat <- data.frame(GFSTRESC = c("K103N", "ND", NA_character_), stringsAsFactors = FALSE)

  skipped <- parse_hiv_resistance_mutations(dat, result_var = GFSTRESC)
  expect_equal(nrow(skipped), 1L)

  kept <- parse_hiv_resistance_mutations(dat, result_var = GFSTRESC, keep_missing = TRUE)
  expect_equal(nrow(kept), 3L)
  expect_true(is.na(kept$MUTATION[[2]]))
  expect_true(is.na(kept$MUTATION[[3]]))
})

test_that("parse_hiv_resistance_mutations errors on malformed mutation syntax", {
  dat <- data.frame(GFSTRESC = "103N", stringsAsFactors = FALSE)

  expect_error(
    parse_hiv_resistance_mutations(dat, result_var = GFSTRESC),
    "missing reference amino acid"
  )
})

test_that("parse_hiv_resistance_mutations errors on ambiguous compact substitutions unless split", {
  dat <- data.frame(GFSTRESC = "V11IL", stringsAsFactors = FALSE)

  expect_error(
    parse_hiv_resistance_mutations(dat, result_var = GFSTRESC),
    "ambiguous"
  )

  split <- parse_hiv_resistance_mutations(
    dat,
    result_var = GFSTRESC,
    split_compact_substitutions = TRUE
  )
  expect_equal(split$AAS, c("I", "L"))
})

test_that("parse_hiv_resistance_mutations maps region when region_map is supplied", {
  dat <- data.frame(
    GFSTRESC = "K103N",
    REGION = "US",
    stringsAsFactors = FALSE
  )
  region_map <- data.frame(
    REGION = "US",
    GENROICD = "NAM",
    GENROI = "North America",
    stringsAsFactors = FALSE
  )

  result <- parse_hiv_resistance_mutations(
    dat,
    result_var = GFSTRESC,
    region_var = REGION,
    region_map = region_map
  )

  expect_equal(result$GENROICD, "NAM")
  expect_equal(result$GENROI, "North America")
})
