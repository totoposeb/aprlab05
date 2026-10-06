#' Get Pokémon Information
#'
#' Retrieve the main data for one or more Pokémon.
#'
#' @param names A character vector of Pokémon names.
#' @return A data frame with one row per Pokémon.
#' @export
#' @examplesIf httr2::is_online()
#' get_pokemon(c("pikachu", "fuecoco"))
get_pokemon <- function(names) {
  # Validates inputs
  stopifnot(
    "`names` should be a character vector!" = is.character(names) &&
      length(names) >= 1 && !anyNA(names) && all(nzchar(trimws(names)))
  )

  # Gets a list of Pokémon data by calling the pokemon RC and the data() method
  pokemon_data <- lapply(names, \(name) pokemon$new(name)$data())

  # Creates a data.frame with the data of all Pokémon in names
  do.call(rbind, pokemon_data)
}