# type_matchup(): with fake responses both Pokemon are Fuecoco (fire)

test_that("type_matchup returns one row with attack and defense", {
  httr2::local_mocked_responses(fake_api)
  result <- type_matchup("one", "two")

  expect_equal(result$attack, 0.5)
  expect_equal(result$defense, 0.5)
})

test_that("type_matchup rejects wrong input", {
  expect_error(type_matchup(25, "pikachu"))
  expect_error(type_matchup("pikachu", 25))
})

test_that("type_matchup gives the known values from the live API", {
  skip_if_offline()
  result <- type_matchup("pikachu", "charizard")

  expect_equal(result$attack, 2)
  expect_equal(result$defense, 1)
})
