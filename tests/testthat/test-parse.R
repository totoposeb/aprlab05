# Parsers: called directly on the fake lists from helper.R (no internet)

test_that("parse_pokemon reads the fake answer", {
  parsed <- parse_pokemon(pokemon_response)

  expect_equal(parsed$name, "fuecoco")
  expect_equal(parsed$types, "fire")
  expect_equal(parsed$abilities, c("blaze", "unaware"))
  expect_equal(parsed$species, "fuecoco")
})

test_that("parse_pokemon returns NA for missing values", {
  response <- pokemon_response
  response$base_experience <- NULL
  response$sprites <- list()
  parsed <- parse_pokemon(response)

  expect_true(is.na(parsed$base_experience))
  expect_true(is.na(parsed$sprite))
})

test_that("parse_species reads the fake answer", {
  parsed <- parse_species(species_response)

  expect_equal(parsed$rarity_level, "Rare")
  expect_true(is.na(parsed$evolves_from_species))
})

test_that("parse_species gives the right rarity level", {
  response <- species_response

  response$capture_rate <- 50
  expect_equal(parse_species(response)$rarity_level, "Rare")

  response$capture_rate <- 51
  expect_equal(parse_species(response)$rarity_level, "Uncommon")

  response$capture_rate <- 151
  expect_equal(parse_species(response)$rarity_level, "Common")

  response$is_legendary <- TRUE
  expect_equal(parse_species(response)$rarity_level, "Legendary")
})

test_that("parse_type returns the attack and defense matrix", {
  parsed <- parse_type(type_response)

  expect_equal(dim(parsed), c(18, 2))
  expect_equal(parsed["grass", "attack"], 2)
})
