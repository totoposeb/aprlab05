# list_pokemon(): the big query, against the live API

test_that("list_pokemon rejects a wrong type", {
  expect_error(list_pokemon(25))
})

test_that("list_pokemon returns the Pokemon of one type", {
  skip_if_offline()
  result <- list_pokemon("electric")

  expect_true("pikachu" %in% result)
})

test_that("list_pokemon returns every name", {
  skip_if_offline()
  result <- list_pokemon()

  expect_true(length(result) > 1000)
  expect_equal(anyDuplicated(result), 0)
})
