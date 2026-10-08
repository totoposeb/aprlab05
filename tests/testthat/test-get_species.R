# get_species(): with fake responses every name gives Fuecoco

test_that("get_species returns one row per name", {
  httr2::local_mocked_responses(fake_api)
  result <- get_species(c("one", "two"))

  expect_equal(dim(result), c(2, 8))
  expect_equal(result$rarity_level, c("Rare", "Rare"))
})

test_that("get_species rejects wrong input", {
  expect_error(get_species(25))
})
