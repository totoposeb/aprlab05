# The pokemon class, built from the fake responses in helper.R

test_that("a pokemon object builds from a fake response", {
  httr2::local_mocked_responses(fake_api)
  p <- pokemon$new("anything")

  expect_equal(p$name, "fuecoco")
  expect_equal(sum(p$stats), 310)
})

test_that("pokemon$new rejects a wrong name", {
  expect_error(pokemon$new(25))
  expect_error(pokemon$new(c("pikachu", "eevee")))
})

test_that("data() returns one row", {
  httr2::local_mocked_responses(fake_api)
  d <- pokemon$new("anything")$data()

  expect_equal(dim(d), c(1, 14))
  expect_equal(d$stats_total, 310)
})

test_that("show() prints the name", {
  httr2::local_mocked_responses(fake_api)
  p <- pokemon$new("anything")

  expect_output(p$show(), "FUECOCO")
})

test_that("get_type_effectiveness() returns the 18 x 2 matrix", {
  httr2::local_mocked_responses(fake_api)
  m <- pokemon$new("anything")$get_type_effectiveness()

  expect_equal(dim(m), c(18, 2))
  expect_equal(m["water", "defense"], 2)
})

test_that("get_species_info() fills the species fields", {
  httr2::local_mocked_responses(fake_api)
  p <- pokemon$new("anything")
  info <- p$get_species_info()

  expect_equal(length(info), 8)
  expect_equal(p$rarity_level, "Rare")
})

test_that("plot_stats() returns a ggplot", {
  httr2::local_mocked_responses(fake_api)
  p <- pokemon$new("anything")
  p$sprite <- NA_character_ # no sprite, so no picture is downloaded

  grDevices::pdf(NULL) # draws to nowhere, so no file is created
  plot <- p$plot_stats()
  grDevices::dev.off()

  expect_true(ggplot2::is_ggplot(plot))
})

test_that("show_sprite() and show_image() stop without a picture", {
  httr2::local_mocked_responses(fake_api)
  p <- pokemon$new("anything")
  p$sprite <- NA_character_
  p$image <- NA_character_

  expect_error(p$show_sprite())
  expect_error(p$show_image())
})

test_that("a real Pokemon builds from the live API", {
  skip_if_offline()
  p <- pokemon$new("bulbasaur")

  expect_equal(p$id, 1)
  expect_equal(p$types, c("grass", "poison"))
  expect_equal(p$get_type_effectiveness()["grass", "defense"], 0.25)
})
