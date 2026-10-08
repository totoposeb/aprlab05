# get_pokemon(): with fake responses every name gives Fuecoco

test_that("get_pokemon returns one row per name", {
  httr2::local_mocked_responses(fake_api)
  result <- get_pokemon(c("one", "two"))

  expect_equal(dim(result), c(2, 14))
})

test_that("get_pokemon rejects wrong input", {
  expect_error(get_pokemon(25))
  expect_error(get_pokemon(character(0)))
})

test_that("get_pokemon keeps the order of the names (live API)", {
  skip_if_offline()
  result <- get_pokemon(c("pikachu", "bulbasaur", "eevee"))

  expect_equal(result$name, c("pikachu", "bulbasaur", "eevee"))
})
